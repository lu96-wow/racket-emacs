#lang racket

;; api/window-ops.rkt — Window commands
;;
;; Window commands receive (buf evt) like all others,
;; but access frame/leaf via global state.

(require "../kernel/buffer.rkt"
         "../display/window.rkt"
         "../display/registry.rkt"
         "../display/render.rkt"
         "../display/dirty.rkt"
         "command.rkt")

(provide
 cmd-other-window
 cmd-split-window-below cmd-split-window-right
 cmd-delete-window cmd-delete-other-windows)

(define-command cmd-other-window "other-window" (buf evt)
  (define frm (current-frame))
  (when frm
    (define lf (frame-selected frm))
    (define next (next-leaf frm))
    (when (and next (not (eq? next lf)))
      (define next-buf (leaf-buffer next))
      (when next-buf (set-buffer next-buf))
      (set-buffer-point! next-buf (leaf-point next))
      (set-frame-selected! frm next)
      (mark-redisplay-needed!)
      (invalidate-frame-cache! frm))))

(define-command cmd-split-window-below "split-window-below" (buf evt)
  (define frm (current-frame))
  (when frm
    (define lf (frame-selected frm))
    (define new (split-leaf! frm lf buf 'vertical))
    (set-frame-selected! frm new)
    (mark-redisplay-needed!)
    (invalidate-frame-cache! frm)))

(define-command cmd-split-window-right "split-window-right" (buf evt)
  (define frm (current-frame))
  (when frm
    (define lf (frame-selected frm))
    (define new (split-leaf! frm lf buf 'horizontal))
    (set-frame-selected! frm new)
    (mark-redisplay-needed!)
    (invalidate-frame-cache! frm)))

(define-command cmd-delete-window "delete-window" (buf evt)
  (define frm (current-frame))
  (when frm
    (define lf (frame-selected frm))
    (define leaves (frame-leaf-list frm))
    (when (> (length leaves) 1)
      (delete-leaf! frm lf)
      (set-buffer (leaf-buffer (frame-selected frm)))
      (mark-redisplay-needed!)
      (invalidate-frame-cache! frm))))

(define-command cmd-delete-other-windows "delete-other-windows" (buf evt)
  (define frm (current-frame))
  (when frm
    (define lf (frame-selected frm))
    (for ([w (in-list (frame-leaf-list frm))])
      (unless (eq? w lf) (delete-leaf! frm w)))
    (mark-redisplay-needed!)
    (invalidate-frame-cache! frm)))
