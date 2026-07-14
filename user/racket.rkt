#lang racket

;; user/racket.rkt — Racket mode

(require "../kernel/syntax.rkt"
         "mode.rkt"
         "racket-keywords.rkt"
         "fundamental.rkt")

(provide racket-mode init-racket-mode!)

(define racket-mode
  (define-mode 'Racket
    #:keymap fundamental-keymap
    #:syntax (make-lisp-syntax-table)
    #:highlight-kw racket-font-lock-keywords
    #:highlight-syntax? #t
    #:file-types '(".rkt")))

(define (init-racket-mode!) (void))
