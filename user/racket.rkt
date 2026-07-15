#lang racket

;; user/racket.rkt — Racket mode: Lisp syntax + font-lock keywords

(require "../kernel/syntax.rkt"
         "../kernel/buffer.rkt"
         "../kernel/font-lock.rkt"
         "../kernel/keymap.rkt"
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
  ;; multi-character comment: block  (nestable)
  (set-syntax-table-multi-rules! table
    (list (multi-char-rule 'block-comment (~a #\# #\|) (~a #\| #\#) #t #f)
          (multi-char-rule 'heredoc      "#<<"               #f               #f #t)))
  table)

;; ── setup function ──

(define (setup-racket! buf)
  (set-buffer-keymap! buf fundamental-keymap)
  (set-buffer-syntax! buf (make-lisp-syntax-table))
  (set-buffer-highlight-keywords! buf racket-font-lock-keywords)
  (set-buffer-highlight-syntax?! buf #t)
  (set-buffer-mode-name! buf 'Racket)
  ;; Racket mode: add rainbow paren-depth pass to the default syntax+keywords
  (set-buffer-fontify-passes! buf
    (append (buffer-fontify-passes buf) (list fontify-paren-depth-pass)))
  (activate-highlight! buf))

(register-mode-setup! 'Racket setup-racket! '(".rkt"))
