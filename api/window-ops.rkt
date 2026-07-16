#lang racket

;; api/window-ops.rkt — Window operations
;;
;; Split, switch, delete windows.  These manipulate the window tree
;; and manage the point-per-window invariant.

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

(define-command cmd-other-window "other-window" (win frm evt)
  (define wins (frame-window-list frm))
  (when (>= (length wins) 2)
    ;; Find next window cyclically
    (define idx (or (index-of wins win) 0))
    (define next (list-ref wins (modulo (add1 idx) (length wins))))
    ;; Switch buffer point to new window's buffer + point
    (define next-buf (window-buffer next))
    (when (and next-buf (not (eq? next-buf (window-buffer win))))
      (set-buffer next-buf))
    (set-buffer-point! next-buf (window-point next))
    ;; Update selection
    (set-window-selected?! win #f)
    (set-window-selected?! next #t)
    (set-frame-selected-window! frm next)
    ;; Scroll new window to show its point
    (recenter-point! next)))

;; ============================================================
;; cmd-split-window-below — horizontal split
;; ============================================================

(define-command cmd-split-window-below "split-window-below" (win frm evt)
  ;; Replace win with a vertical internal window containing win + new leaf
  (define buf (window-buffer win))
  (define new-win (make-leaf-window buf))
  ;; Create internal window
  (define internal (make-internal-window #f)) ; vertical
  (set-window-children! internal (list win new-win))
  ;; Replace win in the tree
  (define parent (window-parent win))
  (define siblings (and parent (window-children parent)))
  (when parent
    (define idx (index-of siblings win))
    (set-window-children! parent
      (list-update siblings idx internal))
    (set-window-parent! internal parent)
    (set-window-parent! win internal)
    (set-window-parent! new-win internal))
  ;; Re-layout and select new window
  (layout-frame! frm)
  (set-window-selected?! win #f)
  (set-window-selected?! new-win #t)
  (set-frame-selected-window! frm new-win)
  (invalidate-frame-cache! frm))

;; ============================================================
;; cmd-split-window-right — vertical split
;; ============================================================

(define-command cmd-split-window-right "split-window-right" (win frm evt)
  (define buf (window-buffer win))
  (define new-win (make-leaf-window buf))
  (define internal (make-internal-window #t)) ; horizontal
  (set-window-children! internal (list win new-win))
  (define parent (window-parent win))
  (define siblings (and parent (window-children parent)))
  (when parent
    (define idx (index-of siblings win))
    (set-window-children! parent
      (list-update siblings idx internal))
    (set-window-parent! internal parent)
    (set-window-parent! win internal)
    (set-window-parent! new-win internal))
  (layout-frame! frm)
  (set-window-selected?! win #f)
  (set-window-selected?! new-win #t)
  (set-frame-selected-window! frm new-win)
  (invalidate-frame-cache! frm))

;; ============================================================
;; Helper
;; ============================================================

(define (index-of lst item)
  (let loop ([xs lst] [i 0])
    (cond [(null? xs) #f]
          [(eq? (car xs) item) i]
          [else (loop (cdr xs) (add1 i))])))

(define (list-update lst idx new)
  (define pre (take lst idx))
  (define post (drop lst (add1 idx)))
  (append pre (list new) post))
