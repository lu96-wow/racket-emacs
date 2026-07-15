#lang racket

;; user/font-lock-activate.rkt — Font-lock activation for a buffer

(require "../kernel/buffer.rkt"
         "../kernel/textprop.rkt"
         "../base/font-lock.rkt"
         "../base/syntax-cache.rkt")

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
    ;; syntax-cache invalidation runs before fontification so the cache
    ;; is stale for positions >= the edit point before font-lock queries it.
    (define cache-invalidate-hook
      (λ (b start lendel lenins) (syntax-cache-invalidate! b start)))
    (unless (memq cache-invalidate-hook (hook-manager-after-fns hm))
      (set-hook-manager-after-fns! hm
        (append (hook-manager-after-fns hm) (list cache-invalidate-hook))))
    ;; font-lock re-fontification runs after cache invalidation.
    (unless (memq fontify-after-change! (hook-manager-after-fns hm))
      (set-hook-manager-after-fns! hm
        (append (hook-manager-after-fns hm) (list fontify-after-change!))))))
