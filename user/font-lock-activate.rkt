#lang racket

;; user/font-lock-activate.rkt — Font-lock activation for a buffer

(require "../kernel/buffer.rkt"
         "../kernel/textprop.rkt"
         "../kernel/font-lock.rkt"
         data/interval-map)

(provide activate-highlight!)

;; ── paren-depth interval-map adjustment ──
;; Runs BEFORE fontify-after-change! on every buffer edit so that
;; paren-depth data stays in sync with the text positions.
(define (paren-depth-adjust! b start lendel lenins)
  (define pd (paren-depth-map b))
  (cond [(positive? lenins)
         (interval-map-expand! pd start (+ start lenins))]
        [(positive? lendel)
         (interval-map-contract! pd start (+ start lendel))]
        [else (void)]))

(define (activate-highlight! buf)
  (define kw (buffer-highlight-keywords buf))
  (unless (null? kw)
    (init-buffer-text-properties! buf)
    (set-font-lock-defaults! kw (buffer-highlight-syntax? buf) #f buf)
    (fontify-buffer! buf)
    (define hm (buffer-hooks buf))
    ;; paren-depth adjustment MUST run before font-lock rebuilds.
    ;; Prepend so it runs first.
    (unless (memq paren-depth-adjust! (hook-manager-after-fns hm))
      (set-hook-manager-after-fns! hm
        (cons paren-depth-adjust! (hook-manager-after-fns hm))))
    ;; font-lock re-fontification runs after interval adjustments.
    (unless (memq fontify-after-change! (hook-manager-after-fns hm))
      (set-hook-manager-after-fns! hm
        (append (hook-manager-after-fns hm) (list fontify-after-change!))))))
