#lang racket

;; lang/font-lock.rkt — Syntax highlighting via racket-lexer
;;
;; ============================================================================
;; Language-layer: pure data + function application.
;; Depends on kernel (gap-buffer, face raw ops) but NOT on display.
;;
;; Face registration (colors) is a display concern — the caller resolves
;; face-names → face-ids via the display layer and passes them in.
;;
;; ============================================================================
;; Architecture
;; ============================================================================
;;
;;   lang/font-lock.rkt        ← this file
;;     → kernel/data/gap.rkt   (gap-length, gap-substring)
;;     → kernel/data/query.rkt (gap-skip-n, gap-scan-byte)
;;     → kernel/data/face.rkt  (face-fill!)
;;     → syntax-color/racket-lexer
;;
;;   Does NOT import display/face.rkt.
;;
;;   Exports:
;;     - face-name constants      (pure symbols)
;;     - token→face-name mapping  (pure data)
;;     - racket-keywords          (pure set)
;;     - font-locker struct       (stores resolved face-ids)
;;     - make-font-locker         (takes resolve: symbol→face-id)
;;     - font-lock-scan-range!    (writes face-ids to gap buffer)
;;     - font-lock-update!        (±15-line re-scan after edit)
;;
;;   Caller (main.rkt) wires:
;;     1. Register faces with display/face using the exported face specs
;;     2. Create font-locker with (make-font-locker face-id-for-name)
;;     3. Call font-lock-scan-range! / font-lock-update!
;;
;; ============================================================================
;; Character → Byte mapping
;; ============================================================================
;;
;; The lexer works with CHARACTER positions (1-based) in a Racket string.
;; The gap buffer stores UTF-8 BYTES.  We extract a substring from the
;; scan range, lex it, and map character positions back to byte positions
;; using gap-skip-n with progressive tracking (small O(1) skips per token).
;;
;; ============================================================================

(require syntax-color/racket-lexer
         "../kernel/data/gap.rkt"
         "../kernel/data/query.rkt"
         "../kernel/data/face.rkt")

(provide
 ;; ── face-name data (caller uses these with display/face to register colors) ──
 font-lock-face-name      ; token-type → face-name symbol
 font-lock-keyword-face-name

 ;; ── face spec data for registration ──
 font-lock-face-specs     ; (listof (list/c symbol? key value ...))

 ;; ── language data ──
 racket-keywords           ; (setof string?)

 ;; ── font-locker struct ──
 font-locker? make-font-locker
 font-locker-token->fid font-locker-keyword-fid font-locker-keywords

 ;; ── scanning ──
 font-lock-scan-range!
 font-lock-update!)

;; ============================================================
;; Face-name constants — pure symbols, no display dependency
;; ============================================================

(define font-lock-comment-face-name  'font-lock-comment-face)
(define font-lock-string-face-name   'font-lock-string-face)
(define font-lock-constant-face-name 'font-lock-constant-face)
(define font-lock-keyword-face-name  'font-lock-keyword-face)

(define font-lock-face-name
  (hasheq 'comment             font-lock-comment-face-name
          'string              font-lock-string-face-name
          'constant            font-lock-constant-face-name
          'hash-colon-keyword  font-lock-keyword-face-name
          'keyword             font-lock-keyword-face-name))

;; ============================================================
;; Face specs — pure data for the display layer to register
;; ============================================================

(define font-lock-face-specs
  `((,font-lock-comment-face-name  foreground (130 130 130)  slant italic)
    (,font-lock-string-face-name   foreground (80  200 120))
    (,font-lock-constant-face-name foreground (200 160 80))
    (,font-lock-keyword-face-name  foreground (100 180 255)  weight bold)))

;; ============================================================
;; Racket keywords — symbols that get keyword face
;; ============================================================

(define racket-keywords
  (list->set
   (list "define" "lambda" "λ" "let" "let*" "letrec" "if" "cond" "case"
         "and" "or" "when" "unless" "begin" "set!" "quote" "quasiquote"
         "unquote" "unquote-splicing" "syntax" "quasisyntax"
         "parameterize" "dynamic-wind" "define-syntax" "struct"
         "module" "module+" "module*" "require" "provide"
         "all-defined-out" "all-from-out" "except-out" "rename-out"
         "prefix-out" "only-in" "except-in" "rename-in" "prefix-in"
         "for" "for/list" "for/vector" "for/hash" "for/and" "for/or"
         "for/sum" "for/fold" "match" "match-define" "match-let"
         "match-lambda" "class" "class*" "interface" "mixin" "trait"
         "new" "send" "field" "init-field" "define/public"
         "define/private" "define/override" "contract-out"
         "->" "->*" "->i" "->d" "or/c" "and/c" "listof" "vectorof"
         "cons/c" "hash/c" "any/c" "none/c" "string?" "number?"
         "boolean?" "symbol?" "procedure?" "cons" "car" "cdr"
         "list" "append" "reverse" "length" "map" "filter"
         "foldl" "foldr" "andmap" "ormap" "memq" "memv" "member"
         "assq" "assv" "assoc" "remove" "sort" "add1" "sub1"
         "zero?" "positive?" "negative?" "even?" "odd?"
         "integer?" "rational?" "real?" "complex?"
         "display" "displayln" "print"
         "printf" "write" "read" "read-line" "open-input-file"
         "open-output-file" "call-with-input-file"
         "call-with-output-file" "port->string" "file->string"
         "current-input-port" "current-output-port" "eof")))

;; ============================================================
;; Struct
;; ============================================================

;; font-locker — resolved face-ids ready for direct writing to gap buffer.
;;
;;   token->fid   — hash: token-type-symbol → face-id
;;   keyword-fid  — face-id for recognized Racket keywords
;;   keywords     — set of keyword strings

(struct font-locker
  (token->fid
   keyword-fid
   keywords)
  #:transparent)

;; ============================================================
;; Constructor — takes face-name→face-id resolver (from display layer)
;; ============================================================

(define (make-font-locker resolve-fid)
  (unless (procedure? resolve-fid)
    (raise-argument-error 'make-font-locker "procedure?" resolve-fid))
  (define t->fid
    (for/hash ([(type name) (in-hash font-lock-face-name)])
      (values type (resolve-fid name))))
  (define kw-fid (resolve-fid font-lock-keyword-face-name))
  (font-locker t->fid kw-fid racket-keywords))

;; ============================================================
;; token→face-id resolution (pure data lookup)
;; ============================================================

(define (token-face-id fl type lexeme)
  (cond
    [(hash-ref (font-locker-token->fid fl) type #f)]
    [(and (eq? type 'symbol)
          (set-member? (font-locker-keywords fl) lexeme))
     (font-locker-keyword-fid fl)]
    [else 0]))

;; ============================================================
;; font-lock-scan-range! — scan [start, end) and write face-ids
;; ============================================================

(define (font-lock-scan-range! fl gb start end)
  (unless (font-locker? fl)
    (raise-argument-error 'font-lock-scan-range! "font-locker?" fl))

  (define buflen (gap-length gb))
  (define real-start (max 0 start))
  (define real-end   (min buflen end))

  (when (< real-start real-end)
    ;; Extract text for the lexer
    (define text (gap-substring gb real-start real-end))
    (define in (open-input-string text))
    (port-count-lines! in)

    ;; Lex and apply faces
    (let lex-loop ([prev-char-end 0] [prev-bp real-start])
      (define-values (lexeme type paren char-start char-end)
        (racket-lexer in))
      (unless (eq? type 'eof)
        (define char-start0 (sub1 char-start))  ; 0-based
        (define char-end0   (sub1 char-end))

        ;; Map character positions to byte positions in gap buffer.
        ;; Uses small progressive skips: each token is only a few characters
        ;; past the previous one, so gap-skip-n does O(1) work per token.
        (define bp-start
          (if (= char-start0 prev-char-end)
              prev-bp
              (gap-skip-n gb prev-bp (- char-start0 prev-char-end))))
        (define bp-end
          (gap-skip-n gb bp-start (- char-end0 char-start0)))

        ;; Write face-id for this token (only if meaningful)
        (define fid (token-face-id fl type lexeme))
        (when (and (positive? fid) (< bp-start bp-end))
          (face-fill! gb bp-start bp-end fid))

        (lex-loop char-end0 bp-end)))))

;; ============================================================
;; font-lock-update! — extend range ±15 lines then scan
;; ============================================================

(define (font-lock-update! fl gb edit-start delta)
  (unless (font-locker? fl)
    (raise-argument-error 'font-lock-update! "font-locker?" fl))

  (define edit-len (max 1 (abs delta)))
  (define-values (scan-start scan-end)
    (extend-scan-range gb edit-start edit-len))

  (font-lock-scan-range! fl gb scan-start scan-end))

;; ============================================================
;; extend-scan-range — ±15 lines around edit
;; ============================================================

(define (extend-scan-range gb edit-start edit-len)
  (define buflen (gap-length gb))
  (define lines 15)
  (define edit-end (min buflen (+ edit-start edit-len)))

  (define sol
    (let loop ([pos edit-start] [remaining lines])
      (if (or (zero? pos) (zero? remaining))
          pos
          (let ([prev-nl (gap-scan-byte gb (sub1 pos) 'backward
                                        (λ (b) (= b #x0A)))])
            (if (>= prev-nl 0)
                (loop prev-nl (sub1 remaining))
                0)))))

  (define eol
    (let loop ([pos edit-end] [remaining lines])
      (define nl (gap-scan-byte gb pos 'forward (λ (b) (= b #x0A))))
      (if (or (>= nl buflen) (zero? remaining))
          (min nl buflen)
          (loop (add1 nl) (sub1 remaining)))))

  (values sol eol))
