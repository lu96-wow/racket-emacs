#lang racket

;; user/racket.rkt — Racket mode

(require "../kernel/syntax.rkt"
         "standard-syntax.rkt"
         "mode.rkt"
         "racket-keywords.rkt"
         "fundamental.rkt")

(provide racket-mode init-racket-mode!)

(define (make-lisp-syntax-table)
  (define table (make-syntax-table (make-standard-syntax-table)))
  (define classes (syntax-table-classes table))
  (hash-set! classes #\' 'expression-prefix)
  (hash-set! classes #\` 'expression-prefix)
  (hash-set! classes #\, 'expression-prefix)
  (hash-set! classes #\# 'expression-prefix)
  (hash-set! classes #\; 'comment-start)
  (hash-set! classes #\| 'symbol)
  table)

(define racket-mode
  (define-mode 'Racket
    #:keymap fundamental-keymap
    #:syntax (make-lisp-syntax-table)
    #:highlight-kw racket-font-lock-keywords
    #:highlight-syntax? #t
    #:file-types '(".rkt")))

(define (init-racket-mode!) (void))
