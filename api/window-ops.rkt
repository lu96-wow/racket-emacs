#lang racket

;; api/window-ops.rkt — Window commands
;;
;; Each command is a pure function composed from:
;;   focus-list / next-leaf  (navigation)
;;   split-leaf! / delete-leaf!  (tree mutation)
;;   layout-frame!  (recalc)

(require "../kernel/buffer.rkt"
         "../display/window.rkt"
         "../display/render.rkt"
         "../display/registry.rkt"
         "command.rkt")

(provide
 cmd-other-window
 cmd-split-window-below cmd-split-window-right
 cmd-delete-window cmd-delete-other-windows)

;; ============================================================
;; Navigation
;; ============================================================

(define-command cmd-other-window "other-window" (lf frm evt)
  (define next (next-leaf frm))
  (when (and next (not (eq? next lf)))
    (define next-buf (leaf-buffer next))
    (when next-buf (set-buffer next-buf))
    (set-buffer-point! next-buf (leaf-point next))
    (set-frame-selected! frm next)
    (invalidate-frame-cache! frm)))

;; ============================================================
;; Split
;; ============================================================

(define-command cmd-split-window-below "split-window-below" (lf frm evt)
  (define buf (leaf-buffer lf))
  (define new (split-leaf! frm lf buf 'vertical))
  (set-frame-selected! frm new)
  (invalidate-frame-cache! frm))

(define-command cmd-split-window-right "split-window-right" (lf frm evt)
  (define buf (leaf-buffer lf))
  (define new (split-leaf! frm lf buf 'horizontal))
  (set-frame-selected! frm new)
  (invalidate-frame-cache! frm))

;; ============================================================
;; Delete
;; ============================================================

(define-command cmd-delete-window "delete-window" (lf frm evt)
  (define leaves (frame-leaf-list frm))
  (when (> (length leaves) 1)
    (delete-leaf! frm lf)
    (set-buffer (leaf-buffer (frame-selected frm)))
    (invalidate-frame-cache! frm)))

(define-command cmd-delete-other-windows "delete-other-windows" (lf frm evt)
  (for ([w (in-list (frame-leaf-list frm))])
    (unless (eq? w lf) (delete-leaf! frm w)))
  (invalidate-frame-cache! frm))
