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
    (fontify-buffer! buf)
    (define hm (buffer-hooks buf))
    ;; paren-depth interval-map adjustment MUST run before font-lock rebuilds.
    (unless (memq paren-depth-adjust! (hook-manager-after-fns hm))
      (set-hook-manager-after-fns! hm
        (cons paren-depth-adjust! (hook-manager-after-fns hm))))
    ;; font-lock re-fontification runs after interval adjustments.
    (unless (memq fontify-after-change! (hook-manager-after-fns hm))
      (set-hook-manager-after-fns! hm
        (append (hook-manager-after-fns hm) (list fontify-after-change!))))))
