#lang racket

;; api/window-ops.rkt — Window operations
;;
;; Commands for window navigation, split, resize, delete, balance.

(require "../kernel/buffer.rkt"
         "../display/window.rkt"
         "../display/registry.rkt"
         "../display/render.rkt"
         "command.rkt")

(provide
 cmd-other-window
 cmd-split-window-below cmd-split-window-right
 cmd-delete-window cmd-delete-other-windows
 cmd-enlarge-window cmd-shrink-window-horizontally cmd-enlarge-window-horizontally
 cmd-balance-windows)

;; ============================================================
;; Navigation
;; ============================================================

(define-command cmd-other-window "other-window" (win frm evt)
  (define wins (frame-window-list frm))
  (when (>= (length wins) 2)
    (define idx (or (index-of wins win) 0))
    (define next (list-ref wins (modulo (add1 idx) (length wins))))
    (define next-buf (window-buffer next))
    (when (and next-buf (not (eq? next-buf (window-buffer win))))
      (set-buffer next-buf))
    (set-buffer-point! next-buf (window-point next))
    (set-window-selected?! win #f)
    (set-window-selected?! next #t)
    (set-frame-selected-window! frm next)
    (recenter-point! next)))

;; ============================================================
;; Split
;; ============================================================

(define-command cmd-split-window-below "split-window-below" (win frm evt)
  (define buf (window-buffer win))
  (define new-win (split-window! win buf #f))
  (set-window-selected?! win #f)
  (set-window-selected?! new-win #t)
  (set-frame-selected-window! frm new-win)
  (invalidate-frame-cache! frm))

(define-command cmd-split-window-right "split-window-right" (win frm evt)
  (define buf (window-buffer win))
  (define new-win (split-window! win buf #t))
  (set-window-selected?! win #f)
  (set-window-selected?! new-win #t)
  (set-frame-selected-window! frm new-win)
  (invalidate-frame-cache! frm))

;; ============================================================
;; Delete
;; ============================================================

(define-command cmd-delete-window "delete-window" (win frm evt)
  (define wins (frame-window-list frm))
  (when (> (length wins) 1)
    (delete-window! win frm)
    (define new-sel (or (selected-window)
                        (car (frame-window-list frm))))
    (set-window-selected?! new-sel #t)
    (set-frame-selected-window! frm new-sel)
    (define buf (window-buffer new-sel))
    (when buf (set-buffer buf))
    (invalidate-frame-cache! frm)))

(define-command cmd-delete-other-windows "delete-other-windows" (win frm evt)
  (define wins (frame-window-list frm))
  (for ([w (in-list wins)])
    (unless (eq? w win) (delete-window! w frm)))
  (invalidate-frame-cache! frm))

;; ============================================================
;; Resize
;; ============================================================

(define-command cmd-enlarge-window "enlarge-window" (win frm evt)
  ;; Enlarge selected window vertically by 1 row
  (adjust-window-size! win frm 'vertical +1))

(define-command cmd-shrink-window-horizontally "shrink-window-horizontally" (win frm evt)
  (adjust-window-size! win frm 'horizontal -1))

(define-command cmd-enlarge-window-horizontally "enlarge-window-horizontally" (win frm evt)
  (adjust-window-size! win frm 'horizontal +1))

(define-command cmd-balance-windows "balance-windows" (win frm evt)
  (balance-windows! frm)
  (invalidate-frame-cache! frm))

;; ============================================================
;; adjust-window-size! — shift ratio between window and its sibling
;; ============================================================

(define (adjust-window-size! win frm direction delta)
  ;; Find the parent internal node and adjust ratios
  (define parent (window-parent win))
  (unless parent
    (error 'adjust-window-size! "cannot resize sole window"))
  (define siblings (window-children parent))
  ;; Only work with exactly 2 children for simplicity
  (unless (= (length siblings) 2)
    ;; For >2 children, pick the next sibling
    (void))
  (when (= (length siblings) 2)
    (define other (if (eq? (car siblings) win) (cadr siblings) (car siblings)))
    (define is-horiz (window-horizontal? parent))
    (define total (if is-horiz (window-cols parent) (window-rows parent)))
    (define my-size (if is-horiz (window-cols win) (window-rows win)))
    (define new-my-size (max 2 (+ my-size delta)))
    (define other-size (if is-horiz (window-cols other) (window-rows other)))
    (define new-other-size (- total new-my-size))
    (when (>= new-other-size 2)
      (set-window-desired-ratio! win (/ new-my-size total 1.0))
      (set-window-desired-ratio! other (/ new-other-size total 1.0))
      (layout-frame! frm)
      (invalidate-frame-cache! frm))))

;; ============================================================
;; Utility
;; ============================================================

(define (index-of lst item)
  (let loop ([xs lst] [i 0])
    (cond [(null? xs) #f]
          [(eq? (car xs) item) i]
          [else (loop (cdr xs) (add1 i))])))
