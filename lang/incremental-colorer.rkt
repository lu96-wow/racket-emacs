#lang racket

;; lang/incremental-colorer.rkt — DrRacket-style incremental syntax coloring
;;
;; Uses syntax-color/token-tree (splay tree) + paren-tree for O(log n)
;; incremental syntax highlighting.  Replaces the full-rescan approach
;; of font-lock.rkt with split/re-lex/merge.
;;
;; Key: this module writes face SYMBOLS to text-props key 'face.
;; bracket-cache writes to 'bracket-face — independent, no conflict.
;;
;; ── Algorithm (adapted from DrRacket's framework/private/color.rkt) ──
;;
;; On edit (colorer-on-edit!):
;;   1. split token-tree at edit point → valid + invalid subtrees
;;   2. store invalid-tokens-start = start-pos + orig-end + change-len
;;   3. store invalid-mode = mode from the split token data
;;   4. clear faces only in the actually-changed byte range
;;   5. set current-pos to resume point, up-to-date? = #f
;;
;; Re-lex (colorer-continue!):
;;   1. create string-port from gap-buffer[current-pos, end]
;;   2. call racket-lexer/status to get one token
;;   3. insert-last-spec! into token-tree, write face to text-props
;;   4. if current-pos (after token) == invalid-start AND mode matches
;;      → merge entire invalid subtree (O(1) convergence!)
;;   5. time budget (20ms) exceeded → yield with callback
;;
;; ── Token data format ──
;;   (vector type status) where:
;;     type   — symbol from lexer (comment, string, parenthesis, symbol, ...)
;;     status — lexer status at end of token (continue, datum, open, close)

(require syntax-color/token-tree
         syntax-color/paren-tree
         syntax-color/racket-lexer
         "../kernel/data/gap.rkt"
         "../kernel/data/query.rkt"
         "../kernel/data/textprop.rkt")

(provide
 ;; state
 colorer-state? make-colorer-state
 colorer-state-needs-work?

 ;; operations
 colorer-full-scan!     ;; initial full buffer scan
 colorer-on-edit!       ;; after buffer mutation
 colorer-continue!       ;; incremental re-lex (call from event loop)

 ;; query
 colorer-token-type-at  ;; classify position

 ;; configuration
 token-type->face-name  ;; parameter: type → face-name
 keyword-face-name      ;; face name for known keywords
 known-keywords)        ;; parameter: set of keyword strings

;; ============================================================
;; Token data — (vector type status)
;; ============================================================

(define (make-token-data type status)
  (vector type status))

(define (token-data-type d)   (vector-ref d 0))
(define (token-data-status d) (vector-ref d 1))

;; ============================================================
;; Face mapping
;; ============================================================

(define token-type->face-name
  (make-parameter
   (hasheq 'comment             'font-lock-comment-face
           'string              'font-lock-string-face
           'constant            'font-lock-constant-face
           'hash-colon-keyword  'font-lock-keyword-face
           'keyword             'font-lock-keyword-face)))

(define keyword-face-name
  (make-parameter 'font-lock-keyword-face))

(define known-keywords
  (make-parameter
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
          "integer?" "rational?" "real?" "complex?" "char?"
          "string?" "bytes?" "list?" "pair?" "null?" "void?"
          "vector?" "hash?" "box?" "display" "displayln" "print"
          "printf" "write" "read" "read-line" "open-input-file"
          "open-output-file" "call-with-input-file"
          "call-with-output-file" "port->string" "file->string"
          "current-input-port" "current-output-port" "eof"))))

(define (face-for-token type lexeme)
  (define direct (hash-ref (token-type->face-name) type #f))
  (if direct
      direct
      (and (eq? type 'symbol)
           (set-member? (known-keywords) lexeme)
           (keyword-face-name))))

;; ============================================================
;; Colorer state
;; ============================================================

(struct colorer-state
  ([tokens #:mutable]         ; token-tree%
   [invalid-tokens #:mutable] ; token-tree%
   [invalid-start #:mutable]  ; byte-pos
   [invalid-mode #:mutable]   ; any/c — lexer mode at invalid-start
   [current-pos #:mutable]    ; byte-pos
   [current-mode #:mutable]   ; any/c
   [parens #:mutable]         ; paren-tree%
   [start-pos #:mutable]      ; byte-pos
   [end-pos #:mutable]        ; byte-pos or 'end
   [up-to-date? #:mutable]    ; boolean?
   [continue-callback #:mutable])
  #:transparent)

(define (make-colorer-state)
  (define parens (new paren-tree% [matches (list (list '|(| '|)|)
                                                  (list '|[| '|]|)
                                                  (list '|{| '|}|))]))
  (colorer-state (new token-tree%) (new token-tree%)
                 +inf.0 #f 0 #f parens 0 'end #t #f))

(define (colorer-state-needs-work? st)
  (not (colorer-state-up-to-date? st)))

;; ============================================================
;; colorer-full-scan! — initial full buffer scan
;; ============================================================

(define (colorer-full-scan! st gb tp)
  (define buflen (gap-length gb))
  (when (positive? buflen)
    (textprop-remove-key! tp 0 buflen 'face)
    (lex-and-apply! st gb tp 0 buflen)))

;; ============================================================
;; colorer-on-edit! — called after buffer mutation
;; ============================================================

(define (colorer-on-edit! st gb tp edit-start change-len)
  ;; edit-start: byte-pos where edit occurred
  ;; change-len: byte delta (> 0 insert, < 0 delete)

  (set-colorer-state-continue-callback! st #f)
  (define st-start (colorer-state-start-pos st))
  (define buflen (gap-length gb))
  ;; Clamp edit-start to valid range
  (define safe-edit-start (max 0 (min edit-start buflen)))

  ;; If already not up-to-date, sync invalid tokens first
  (unless (colorer-state-up-to-date? st)
    (sync-invalid-tokens st))

  (cond
    [(colorer-state-up-to-date? st)
     ;; Split the valid token tree at edit point
     (define split-pos (- safe-edit-start st-start))
     (define-values (orig-start orig-end valid-tree invalid-tree orig-data)
       (if (or (<= split-pos 0) (send (colorer-state-tokens st) is-empty?))
           (values 0 0 (new token-tree%) (colorer-state-invalid-tokens st) #f)
           (send (colorer-state-tokens st) split/data split-pos)))

     ;; Split paren tree at same point (only if position is positive)
     (when (positive? orig-start)
       (send (colorer-state-parens st) split-tree orig-start))

     ;; Store invalid-tokens info
     (set-colorer-state-invalid-tokens! st invalid-tree)
     (set-colorer-state-invalid-start! st
       (if (send invalid-tree is-empty?)
           +inf.0
           (max 0 (+ st-start orig-end change-len))))
     (set-colorer-state-invalid-mode! st
       (and orig-data (token-data-status orig-data)))

     ;; Resume from split point
     (define resume-pos (max 0 (+ st-start orig-start)))
     (set-colorer-state-current-pos! st resume-pos)
     (set-colorer-state-current-mode! st
       (if (or (= resume-pos st-start) (send valid-tree is-empty?))
           #f
           (begin
             (send valid-tree search-max!)
             (let ([d (send valid-tree get-root-data)])
               (and d (token-data-status d))))))
     (set-colorer-state-up-to-date?! st #f)]

    [(and (>= safe-edit-start (colorer-state-invalid-start st))
          (not (send (colorer-state-invalid-tokens st) is-empty?)))
     (let-values ([(tok-start tok-end _vt _it _data)
                    (send (colorer-state-invalid-tokens st)
                          split/data (max 0 (- safe-edit-start
                                              (colorer-state-invalid-start st))))])
       (set-colorer-state-invalid-tokens! st _it)
       (set-colorer-state-invalid-start! st
         (max 0 (+ (colorer-state-invalid-start st) tok-end change-len))))]

    [(> safe-edit-start (colorer-state-current-pos st))
     (set-colorer-state-invalid-start! st
       (max 0 (+ change-len (colorer-state-invalid-start st))))]

    [else
     (let-values ([(tok-start tok-end valid-tree _it _data)
                    (send (colorer-state-tokens st)
                          split/data (max 0 (- safe-edit-start st-start)))])
       (when (positive? tok-start)
         (send (colorer-state-parens st) truncate tok-start))
       (set-colorer-state-invalid-tokens! st _it)
       (set-colorer-state-invalid-start! st
         (max 0 (+ change-len (colorer-state-invalid-start st))))
       (define resume-pos (max 0 (+ st-start tok-start)))
       (set-colorer-state-current-pos! st resume-pos)
       (set-colorer-state-current-mode! st
         (if (or (= resume-pos st-start) (send valid-tree is-empty?))
             #f
             (begin
               (send valid-tree search-max!)
               (let ([d (send valid-tree get-root-data)])
                 (and d (token-data-status d))))))
       (set-colorer-state-up-to-date?! st #f))])

  ;; Clear faces ONLY in the actually-changed byte range
  (define clear-start (max 0 safe-edit-start))
  (define clear-end (min (+ clear-start (max 1 (abs change-len))) buflen))
  (when (< clear-start clear-end)
    (textprop-remove-key! tp clear-start clear-end 'face)))

(define (sync-invalid-tokens st)
  (define inv (colorer-state-invalid-tokens st))
  (define inv-start (colorer-state-invalid-start st))
  (when (and (not (send inv is-empty?))
             (< inv-start (colorer-state-current-pos st)))
    (send inv search-min!)
    (define len (send inv get-root-length))
    (send inv remove-root!)
    (set-colorer-state-invalid-start! st (+ inv-start len))
    (sync-invalid-tokens st)))

;; ============================================================
;; colorer-continue! — incremental re-lex (time-budgeted)
;; ============================================================

(define (colorer-continue! st gb tp [time-budget-ms 20] [yield-callback #f])
  ;; Returns: #t = done, #f = more work needed (callback queued)

  (cond
    [(colorer-state-up-to-date? st)
     (set-colorer-state-continue-callback! st #f)
     #t]
    [else
     (define start-time (current-inexact-milliseconds))
     (define buflen (gap-length gb))
     (define end-limit (if (eq? (colorer-state-end-pos st) 'end)
                           buflen
                           (min (colorer-state-end-pos st) buflen)))
     (define current-pos (colorer-state-current-pos st))

     (cond
       [(>= current-pos end-limit)
        (set-colorer-state-up-to-date?! st #t)
        (set-colorer-state-continue-callback! st #f)
        #t]
       [else
        ;; Create one string port for the un-lexed region
        (define port-text (gap-substring gb current-pos end-limit))
        (define in (open-input-string port-text))
        (port-count-lines! in)
        (define current-mode (colorer-state-current-mode st))
        (define tokens (colorer-state-tokens st))
        (define invalid (colorer-state-invalid-tokens st))
        (define invalid-start (colorer-state-invalid-start st))
        (define invalid-mode (colorer-state-invalid-mode st))

        (let re-lex-loop ([mode current-mode] [ok-to-stop? #f])
          (define-values (lexeme type paren lex-start lex-end status)
            (racket-lexer/status in))
          (cond
            [(eq? type 'eof)
             (set-colorer-state-current-pos! st end-limit)
             (set-colorer-state-current-mode! st mode)
             (set-colorer-state-up-to-date?! st #t)
             (set-colorer-state-continue-callback! st #f)
             #t]
            [else
             (define buf-start (+ current-pos lex-start -1))
             (define buf-end   (+ current-pos lex-end -1))
             (define tok-len (- buf-end buf-start))
             (when (positive? tok-len)
               (insert-last-spec! tokens tok-len (make-token-data type status)))
             (when paren
               (send (colorer-state-parens st) add-token paren tok-len))
             (define face-name (face-for-token type lexeme))
             (when face-name
               (textprop-put! tp buf-start buf-end 'face face-name))
             ;; Merge check: buf-end == invalid-start AND mode matches?
             (cond
               [(and (not (send invalid is-empty?))
                     (= buf-end invalid-start)
                     (equal? status invalid-mode))
                (send invalid search-max!)
                (send (colorer-state-parens st) merge-tree
                      (send invalid get-root-end-position))
                (insert-last! tokens invalid)
                (send tokens search-max!)
                (set-colorer-state-current-pos! st
                  (+ (colorer-state-start-pos st) (send tokens get-root-end-position)))
                (set-colorer-state-current-mode! st
                  (let ([d (send tokens get-root-data)])
                    (and d (token-data-status d))))
                (set-colorer-state-invalid-start! st +inf.0)
                (set-colorer-state-invalid-mode! st #f)
                (set-colorer-state-up-to-date?! st #t)
                (set-colorer-state-continue-callback! st #f)
                #t]
               [else
                (define elapsed (- (current-inexact-milliseconds) start-time))
                (cond
                  [(and ok-to-stop? (> elapsed time-budget-ms))
                   (set-colorer-state-current-pos! st buf-end)
                   (set-colorer-state-current-mode! st status)
                   (set-colorer-state-continue-callback! st yield-callback)
                   (when yield-callback (yield-callback))
                   #f]
                  [else
                   (re-lex-loop status #t)])])]))])]))
;; ============================================================
;; colorer-token-type-at — classify a buffer position
;; ============================================================

(define (colorer-token-type-at st gb pos)
  (define rel-pos (- pos (colorer-state-start-pos st)))
  (when (>= rel-pos 0)
    (send (colorer-state-tokens st) search! rel-pos)
    (define d (send (colorer-state-tokens st) get-root-data))
    (and d (token-data-type d))))

;; ============================================================
;; Internal: lex-and-apply! — scan range and build tree
;; ============================================================

(define (lex-and-apply! st gb tp start-pos end-pos)
  (define buflen (gap-length gb))
  (define real-end (min end-pos buflen))

  (set-colorer-state-start-pos! st start-pos)
  (set-colorer-state-end-pos! st end-pos)
  (send (colorer-state-tokens st) reset-tree)
  (send (colorer-state-invalid-tokens st) reset-tree)
  (set-colorer-state-invalid-start! st +inf.0)
  (set-colorer-state-invalid-mode! st #f)
  (set-colorer-state-current-pos! st start-pos)
  (set-colorer-state-current-mode! st #f)
  (set-colorer-state-up-to-date?! st #f)

  (when (>= real-end start-pos)
    (define port-text (gap-substring gb start-pos real-end))
    (define in (open-input-string port-text))
    (port-count-lines! in)

    (let loop ([mode #f])
      (define-values (lexeme type paren lex-start lex-end status)
        (racket-lexer/status in))
      (unless (eq? type 'eof)
        (define buf-start (+ start-pos lex-start -1))
        (define buf-end   (+ start-pos lex-end -1))
        (define tok-len (- buf-end buf-start))

        (when (positive? tok-len)
          (insert-last-spec! (colorer-state-tokens st) tok-len
                             (make-token-data type status)))

        (when paren
          (send (colorer-state-parens st) add-token paren tok-len))

        (define face-name (face-for-token type lexeme))
        (when face-name
          (textprop-put! tp buf-start buf-end 'face face-name))

        (set-colorer-state-current-pos! st buf-end)
        (set-colorer-state-current-mode! st status)
        (loop status))))

  (set-colorer-state-up-to-date?! st #t)
  (set-colorer-state-continue-callback! st #f))
