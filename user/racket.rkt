#lang racket

;; user/racket.rkt — Racket mode: Lisp syntax + font-lock keywords

(require "../kernel/syntax.rkt"
         "../kernel/buffer.rkt"
         "standard-syntax.rkt"
         "mode.rkt"
         "racket-keywords.rkt"
         "font-lock-activate.rkt"
         "fundamental.rkt")

(provide racket-font-lock-keywords)

;; ── Lisp syntax table ──

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

;; ── setup function ──

(define (setup-racket! buf)
  (set-buffer-keymap! buf fundamental-keymap)
  (set-buffer-syntax! buf (make-lisp-syntax-table))
  (set-buffer-highlight-keywords! buf racket-font-lock-keywords)
  (set-buffer-highlight-syntax?! buf #t)
  (set-buffer-mode-name! buf 'Racket)
  (activate-highlight! buf))

(register-mode-setup! 'Racket setup-racket! '(".rkt"))
