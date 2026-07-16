#lang racket

;; api/bindings.rkt — Default global key bindings

(require "key.rkt"
         "keymap.rkt"
         "command.rkt"
         "navigation.rkt"
         "editing.rkt"
         "window-ops.rkt")

(provide init-global-keymap!)

(define (init-global-keymap!)
  ;; Navigation
  (keymap-set! global-keymap (key 'symbol 'up)        cmd-prev-line)
  (keymap-set! global-keymap (key 'symbol 'down)      cmd-next-line)
  (keymap-set! global-keymap (key 'symbol 'right)     cmd-forward-char)
  (keymap-set! global-keymap (key 'symbol 'left)      cmd-backward-char)
  (keymap-set! global-keymap (key 'symbol 'home)      cmd-beginning-of-line)
  (keymap-set! global-keymap (key 'symbol 'end)       cmd-end-of-line)
  (keymap-set! global-keymap (key 'ctrl #\f)          cmd-forward-char)
  (keymap-set! global-keymap (key 'ctrl #\b)          cmd-backward-char)
  (keymap-set! global-keymap (key 'ctrl #\a)          cmd-beginning-of-line)
  (keymap-set! global-keymap (key 'ctrl #\e)          cmd-end-of-line)
  (keymap-set! global-keymap (key 'ctrl #\p)          cmd-prev-line)
  (keymap-set! global-keymap (key 'ctrl #\n)          cmd-next-line)
  ;; Editing
  (keymap-set! global-keymap (key 'symbol 'return)    cmd-newline)
  (keymap-set! global-keymap (key 'symbol 'tab)       cmd-tab)
  (keymap-set! global-keymap (key 'symbol 'backspace) cmd-backward-delete)
  (keymap-set! global-keymap (key 'ctrl #\d)          cmd-forward-delete)
  (keymap-set! global-keymap (key 'ctrl #\k)          cmd-kill-line)
  (keymap-set! global-keymap (key 'ctrl #\y)          cmd-yank)
  (keymap-set! global-keymap (key 'ctrl #\t)          cmd-toggle-wrap-mode)
  ;; Undo/Redo
  (keymap-set! global-keymap (key 'ctrl #\_)          cmd-undo)
  (keymap-set! global-keymap (key 'ctrl #\r)          cmd-redo)
  ;; Window
  (keymap-set! global-keymap (key 'ctrl #\o)          cmd-other-window)
  (keymap-set! global-keymap (key 'ctrl #\2)          cmd-split-window-below)
  (keymap-set! global-keymap (key 'ctrl #\3)          cmd-split-window-right)
  (keymap-set! global-keymap (key 'ctrl #\0)          cmd-delete-window)
  (keymap-set! global-keymap (key 'ctrl #\1)          cmd-delete-other-windows))
