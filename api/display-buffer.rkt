#lang racket

;; api/display-buffer.rkt — Buffer → window dispatch system
;;
;; Central entry point for showing a buffer.  Users customize window
;; placement via `display-buffer-alist` — a list of (predicate . actions)
;; rules.  Built-in actions: same-window, other-window, below, right.
;;
;; Patterned after Emacs' display-buffer.

(require "../kernel/buffer.rkt"
         "../kernel/text.rkt"
         "../display/window.rkt"
         "../display/render.rkt"
         "../display/registry.rkt"
         "../platform/termios.rkt")

(provide
 ;; actions
 display-action? display-action
 display-action-name display-action-fn
 display-buffer-same-window
 display-buffer-other-window
 display-buffer-below
 display-buffer-right

 ;; alist
 display-buffer-alist

 ;; main entry
 display-buffer)

;; ============================================================
;; Display action struct
;; ============================================================

(struct display-action
  (name    ; symbol — 'same-window 'other-window 'below 'right
   fn)     ; (buffer frame alist) -> window-or-#f
  #:transparent)

;; ============================================================
;; Built-in actions
;; ============================================================

;; Same window — reuse current window (or create frame if none)
(define display-buffer-same-window
  (display-action 'same-window
    (λ (buf frm _alist)
      (define sel (selected-window))
      (if sel
        (begin
          (set-window-buffer! sel buf)
          (set-window-start! sel (text-marker! (buffer-text buf) 0 #f))
          (set-window-pointm! sel (buffer-point-marker buf))
          sel)
        ;; Bootstrap: no frame yet — create one with terminal size
        (let ([new-frm (init-root-frame buf (terminal-width) (terminal-height))])
          (frame-selected-window new-frm))))))

;; Other window — find another visible window
(define display-buffer-other-window
  (display-action 'other-window
    (λ (buf frm _alist)
      (define other (other-visible-window frm))
      (and other
           (begin
             (set-window-buffer! other buf)
             (set-window-start! other (text-marker! (buffer-text buf) 0 #f))
             (set-window-pointm! other (buffer-point-marker buf))
             other)))))

;; Below — split selected window vertically, new buffer below
(define display-buffer-below
  (display-action 'below
    (λ (buf frm _alist)
      (define sel (selected-window))
      (and sel
           (let ([new (split-window! sel buf #f)])
             (set-window-desired-ratio! sel 0.5)
             (set-window-desired-ratio! new 0.5)
             new)))))

;; Right — split selected window horizontally, new buffer right
(define display-buffer-right
  (display-action 'right
    (λ (buf frm _alist)
      (define sel (selected-window))
      (and sel
           (let ([new (split-window! sel buf #t)])
             (set-window-desired-ratio! sel 0.5)
             (set-window-desired-ratio! new 0.5)
             new)))))

;; ============================================================
;; display-buffer-alist — user-customizable rules
;; ============================================================
;;
;; Each entry: (predicate . (list-of display-action))
;; predicate: (buffer -> boolean)
;; First matching predicate wins; its actions are tried in order.

(define display-buffer-alist (make-parameter '()))

;; ============================================================
;; display-buffer — main dispatch
;; ============================================================

(define (display-buffer buf
                        #:action [default-action display-buffer-same-window]
                        #:frame  [initial-frm (current-frame)])
  ;; 1. Ensure buffer is registered
  (register-buffer! buf)

  ;; 2. Try alist rules
  (define matched
    (for/or ([entry (in-list (display-buffer-alist))])
      (match-define (cons pred actions) entry)
      (and (pred buf)
           (for/or ([act (in-list actions)])
             ((display-action-fn act) buf initial-frm '())))))

  ;; 3. Fall back to default action
  (define win (or matched ((display-action-fn default-action) buf initial-frm '())))
  (when win
    ;; Refresh frame — action may have created one via init-root-frame
    (define frm (current-frame))
    (define old-sel (selected-window))
    (when old-sel (set-window-selected?! old-sel #f))
    (set-window-selected?! win #t)
    (set-frame-selected-window! frm win)
    (set-buffer (window-buffer win))
    (invalidate-frame-cache! frm))
  win)
