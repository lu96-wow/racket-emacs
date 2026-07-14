#lang racket

;; user/font-lock-activate.rkt — Font-lock activation for a buffer

(require "../kernel/buffer.rkt"
         "../kernel/textprop.rkt"
         "../kernel/font-lock.rkt")

(provide activate-highlight!)

(define (activate-highlight! buf)
  (define kw (buffer-highlight-keywords buf))
  (unless (null? kw)
    (init-buffer-text-properties! buf)
    (set-font-lock-defaults! kw (buffer-highlight-syntax? buf) #f buf)
    (font-lock-fontify-buffer buf)
    (define hm (buffer-hooks buf))
    (unless (memq font-lock-after-change-handler (hook-manager-after-fns hm))
      (set-hook-manager-after-fns! hm
        (append (hook-manager-after-fns hm) (list font-lock-after-change-handler))))))
