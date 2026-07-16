#lang racket

;; api/window-ops.rkt — Window operations

(require "../kernel/buffer.rkt"
         "../display/window.rkt"
         "../display/registry.rkt"
         "../display/render.rkt"
         "command.rkt")

(provide
 cmd-other-window
 cmd-split-window-below
 cmd-split-window-right)

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

(define-command cmd-split-window-below "split-window-below" (win frm evt)
  (define buf (window-buffer win))
  (define new-win (make-leaf-window buf))
  (define internal (make-internal-window #f))
  (set-window-children! internal (list win new-win))
  (define parent (window-parent win))
  (define siblings (and parent (window-children parent)))
  (when parent
    (define idx (index-of siblings win))
    (set-window-children! parent (list-update siblings idx internal))
    (set-window-parent! internal parent)
    (set-window-parent! win internal)
    (set-window-parent! new-win internal))
  (layout-frame! frm)
  (set-window-selected?! win #f)
  (set-window-selected?! new-win #t)
  (set-frame-selected-window! frm new-win)
  (invalidate-frame-cache! frm))

(define-command cmd-split-window-right "split-window-right" (win frm evt)
  (define buf (window-buffer win))
  (define new-win (make-leaf-window buf))
  (define internal (make-internal-window #t))
  (set-window-children! internal (list win new-win))
  (define parent (window-parent win))
  (define siblings (and parent (window-children parent)))
  (when parent
    (define idx (index-of siblings win))
    (set-window-children! parent (list-update siblings idx internal))
    (set-window-parent! internal parent)
    (set-window-parent! win internal)
    (set-window-parent! new-win internal))
  (layout-frame! frm)
  (set-window-selected?! win #f)
  (set-window-selected?! new-win #t)
  (set-frame-selected-window! frm new-win)
  (invalidate-frame-cache! frm))

(define (index-of lst item)
  (let loop ([xs lst] [i 0])
    (cond [(null? xs) #f]
          [(eq? (car xs) item) i]
          [else (loop (cdr xs) (add1 i))])))

(define (list-update lst idx new)
  (append (take lst idx) (list new) (drop lst (add1 idx))))
