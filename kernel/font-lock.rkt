#lang racket

;; kernel/font-lock.rkt — Syntax highlighting via racket-lexer
;;
;; ============================================================================
;; Leverages Racket's built-in lexer (syntax-color/racket-lexer) for full
;; syntax-aware tokenization.  Writes face-ids directly to the gap buffer's
;; face array — same channel as bracket-colorer.
;;
;; ============================================================================
;; Ordering: bracket-colorer runs FIRST, font-lock runs SECOND.
;; Font-lock overwrites bracket faces where it has its own data
;; (e.g., a `(` inside a string gets string face, not bracket face).
;;
;; ============================================================================
;; Design: simple re-scan (±15 lines around edit), no incremental state.
;; For terminal-sized files (a few thousand lines), a full re-scan of the
;; visible region is fast enough.  Token trees and checkpoint convergence
;; can be added later if profiling shows a bottleneck.
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
;; Dependencies
;; ============================================================================
;;
;;   syntax-color/racket-lexer  — Racket's built-in lexer
;;   kernel/data/gap.rkt        — gap-length, gap-buffer-*
;;   kernel/data/query.rkt      — gap-substring, gap-skip-n, gap-scan-byte
;;   kernel/data/face.rkt       — face-fill!
;;   display/face.rkt           — define-face!, face-id-for-name, make-face-attrs
;;
;; ============================================================================

(require syntax-color/racket-lexer
         "data/gap.rkt"
         "data/query.rkt"
         "data/face.rkt"
         "../display/face.rkt")

(provide
 ;; ── data ──
 font-locker? make-font-locker
 font-locker-keywords

 ;; ── face registration ──
 font-lock-register-faces!

 ;; ── scanning ──
 font-lock-scan-range!
 font-lock-update!)

;; ============================================================
;; Token type → face name mapping
;; ============================================================

(define token->face-name
  (hasheq 'comment             'font-lock-comment-face
          'string              'font-lock-string-face
          'constant            'font-lock-constant-face
          'hash-colon-keyword  'font-lock-keyword-face
          'keyword             'font-lock-keyword-face))

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
;;   token->fid   — hash: token-type-symbol → face-id (comment, string, constant)
;;   keyword-fid  — face-id for recognized Racket keywords
;;   keywords     — set of keyword strings (for symbol lookup)

(struct font-locker
  (token->fid
   keyword-fid
   keywords)
  #:transparent)

;; ============================================================
;; Face registration
;; ============================================================

(define (font-lock-register-faces!)
  (define-face! 'font-lock-comment-face
                (make-face-attrs 'foreground (list 130 130 130)   ; grey
                                 'slant 'italic))
  (define-face! 'font-lock-string-face
                (make-face-attrs 'foreground (list 80 200 120)))  ; green
  (define-face! 'font-lock-constant-face
                (make-face-attrs 'foreground (list 200 160 80)))  ; orange
  (define-face! 'font-lock-keyword-face
                (make-face-attrs 'foreground (list 100 180 255)   ; blue
                                 'weight 'bold)))

;; ============================================================
;; Constructor — resolves face names to face-ids
;; ============================================================

(define (make-font-locker fc)
  (unless fc
    (raise-argument-error 'make-font-locker "face-cache?" fc))
  (define t->fid
    (for/hash ([(type name) (in-hash token->face-name)])
      (values type (face-id-for-name name fc))))
  (define kw-fid (face-id-for-name 'font-lock-keyword-face fc))
  (font-locker t->fid kw-fid racket-keywords))

;; ============================================================
;; token→face-id resolution
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
;; extend-scan-range — shared with bracket-colorer
;; (duplicated to avoid circular dependency)
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
