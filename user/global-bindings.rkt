#lang racket

;; user/global-bindings.rkt — Global keymap bindings

(require "../kernel/buffer.rkt"
         "../kernel/keymap.rkt"
         "../kernel/window.rkt"
         "../base/keybind.rkt"
         "../base/edit.rkt"
         "../base/window-ops.rkt"
         "../base/registry.rkt"
         "minibuffer-loop.rkt"
         "file-io.rkt"
         "../platform/termios.rkt")

(provide init-global-bindings!)

(define (init-global-bindings!)
  (bind-key global-keymap "C-x C-f" (λ ()
    (define path (read-from-minibuffer! "Find file: "))
    (when (and path (not (equal? path "")))
      (define buf (find-file path)) (define win (selected-window))
      (when (and win (not (window-mini? win)) (not (eq? (window-buffer win) buf)))
        (switch-buffer-in-window! win buf)))))
  (bind-key global-keymap "C-x C-s" (λ () (save-buffer)))
  (bind-key global-keymap "C-x b" (λ ()
    (define name (read-from-minibuffer! "Switch to buffer: "))
    (when (and name (not (equal? name "")))
      (define buf (switch-to-buffer name)) (define win (selected-window))
      (when (and win (not (window-mini? win)) (not (eq? (window-buffer win) buf)))
        (switch-buffer-in-window! win buf)))))
  (bind-key global-keymap "C-x 2" (λ () (split-window-below)))
  (bind-key global-keymap "C-x 3" (λ () (split-window-right)))
  (bind-key global-keymap "C-x 0" (λ () (delete-window)))
  (bind-key global-keymap "C-x 1" (λ () (delete-other-windows)))
  (bind-key global-keymap "C-x o" (λ () (other-window)))
  (bind-key global-keymap "C-g" (λ () (deactivate-mark)))
  (bind-key global-keymap "C-x C-c" (λ () (screen-cleanup!) (exit))))
