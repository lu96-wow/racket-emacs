#lang racket

;; user/racket-complete.rkt — Racket code completion

(require "../kernel/buffer.rkt"
         "../kernel/gap.rkt"
         "../kernel/completion.rkt"
         "../kernel/syntax.rkt"
         "../base/edit.rkt")

(provide
 racket-completion-source
 racket-completion-at-point
 completion-at-point
 current-completion-echo)

;; ============================================================
;; racket-completion-source
;; ============================================================

(define racket-keyword-list
  (list "define" "lambda" "λ" "let" "let*" "letrec" "let-values"
        "let*-values" "letrec-values" "let-syntax" "letrec-syntax"
        "local" "shared" "recur" "case-lambda"
        "if" "cond" "else" "when" "unless" "and" "or" "not"
        "case" "match" "begin" "begin0" "set!"
        "for" "for/list" "for/vector" "for/hash"
        "for/fold" "for/and" "for/or"
        "quote" "quasiquote" "unquote" "unquote-splicing"
        "syntax" "quasisyntax"
        "module" "module+" "module*" "require" "provide"
        "all-defined-out" "all-from-out" "rename-out" "except-out" "only-in"
        "struct" "struct-copy" "class" "class*" "interface" "interface*"
        "mixin" "inherit" "init" "field" "public" "private" "override"
        "define-syntax" "define-syntax-rule" "define-syntaxes"
        "syntax-rules" "syntax-case" "with-syntax" "syntax-parse"
        "#t" "#f" "#true" "#false" "null" "empty" "void"
        "cons" "car" "cdr" "list" "append" "reverse" "length"
        "assq" "assv" "member" "memq" "memv" "remove" "remove*" "sort"
        "list-ref" "list-tail"
        "pair?" "null?" "list?" "zero?" "positive?" "negative?"
        "even?" "odd?" "exact?" "inexact?"
        "number?" "string?" "symbol?" "char?" "boolean?" "procedure?"
        "vector?" "hash?" "equal?" "eq?" "eqv?"
        "apply" "map" "filter" "foldl" "foldr"
        "string-append" "string-length" "string-ref" "string->list"
        "string->symbol" "string->number" "string-join" "substring"
        "string?" "make-string" "string-copy" "string-replace"
        "regexp-match" "regexp-match?" "regexp-replace"
        "number->string" "symbol->string" "format" "printf"
        "display" "displayln" "write" "print" "read" "read-line"
        "open-input-file" "open-output-file"
        "call-with-input-file" "call-with-output-file" "port?"
        "with-handlers" "raise" "error"
        "make-parameter" "parameterize" "parameterize*"
        "call/cc" "call-with-current-continuation" "dynamic-wind" "exit"
        "make-hash" "make-hasheq" "make-hasheqv"
        "hash-ref" "hash-set" "hash-remove" "hash-has-key?"
        "hash-count" "hash-keys" "hash-values" "hash-iterate"
        "in-hash" "for/hash" "call-with-values"))

(define (racket-completion-source) racket-keyword-list)

;; ============================================================
;; racket-completion-at-point
;; ============================================================

(define (racket-completion-at-point)
  ;; Returns 3 values: start end source.
  ;; Returns (values #f #f #f) when no completion available.
  (define st (buffer-syntax-table (current-buffer)))
  (define sym (and st (symbol-at-point)))
  (cond
    [(not (and st sym))
     (values #f #f #f)]
    [else
     (define buf (current-buffer))
     (define gb (buffer-gap buf))
     (define pt (buffer-point buf))
     (define buflen (gap-byte-length gb))
     (if (>= pt buflen)
         (values #f #f #f)
         (let ([start (symbol-start gb pt buflen st)]
               [end   (symbol-end   gb pt buflen st)])
           (if (and start end (< start end))
               (values start end (racket-completion-source))
               (values #f #f #f))))]))

(define (symbol-start gb pt buflen st)
  (let loop ([p pt])
    (if (<= p 0)
        0
        (let ([prev (gap-prev-char-pos gb p)])
          (if (and prev
                   (let-values ([(ch _) (gap-char-at gb prev)])
                     (lisp-identifier-char? ch st)))
              (loop prev)
              p)))))

(define (symbol-end gb pt buflen st)
  (let loop ([p pt])
    (if (>= p buflen)
        buflen
        (let-values ([(ch cl) (gap-char-at gb p)])
          (if (lisp-identifier-char? ch st)
              (loop (+ p cl))
              p)))))

;; ============================================================
;; completion-at-point — TAB handler
;; ============================================================

(define (completion-at-point)
  (let-values ([(start end source) (racket-completion-at-point)])
    (when (and source start end)
      (let* ([p (buffer-substring (current-buffer) start end)]
             [candidates (completion-candidates source p)])
        (cond
          [(null? candidates)
           ((current-completion-echo) "[no match]")]
          [(null? (cdr candidates))
           (let ([buf (current-buffer)])
             (buffer-delete buf start end)
             (buffer-insert buf (car candidates) #:at start))]
          [else
           (let* ([n (length candidates)]
                  [msg (if (<= n 5)
                           (format "[~a] ~a" n (string-join candidates " "))
                           (format "[~a matches]" n))])
             ((current-completion-echo) msg))])))))

;; ============================================================
;; Echo — set by main.rkt
;; ============================================================

(define current-completion-echo
  (make-parameter (λ (msg) (void))))
