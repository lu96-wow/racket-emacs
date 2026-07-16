#lang racket

;; api/window-ops.rkt — Window operations
;;
;; Split, switch, delete windows.  These manipulate the window tree
;; and manage the point-per-window invariant.
;; Undo for other-window restores the previously selected window.

(require "../kernel/buffer.rkt"
         "../kernel/text.rkt"
         "../display/window.rkt"
         "../display/registry.rkt"
         "../display/render.rkt"
         "command.rkt")

(provide
 cmd-other-window
 cmd-split-window-below
 cmd-split-window-right)

;; ============================================================
;; cmd-other-window — cycle to next window
;; ============================================================
;; Undo: switch back to the window that was selected before.

(define cmd-other-window
  (command "other-window"
    ;; execute
    (λ (win frm evt)
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
    ;; undo — switch back to previous window
    (λ (win frm prev-win)
      (set-window-selected?! win #f)
      (set-window-selected?! prev-win #t)
      (set-frame-selected-window! frm prev-win)
      (define buf (window-buffer prev-win))
      (set-buffer buf)
      (set-buffer-point! buf (window-point prev-win)))
    ;; state — capture which window is currently selected
    (λ (win frm) win)))

;; ============================================================
;; cmd-split-window-below — no undo (structural change)
;; ============================================================

(define-no-undo-command cmd-split-window-below "split-window-below" (win frm evt)
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

;; ============================================================
;; cmd-split-window-right — no undo (structural change)
;; ============================================================

(define-no-undo-command cmd-split-window-right "split-window-right" (win frm evt)
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

;; ============================================================
;; Helpers
;; ============================================================

(define (index-of lst item)
  (let loop ([xs lst] [i 0])
    (cond [(null? xs) #f]
          [(eq? (car xs) item) i]
          [else (loop (cdr xs) (add1 i))])))

(define (list-update lst idx new)
  (append (take lst idx) (list new) (drop lst (add1 idx))))
