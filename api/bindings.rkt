#lang racket

;; api/bindings.rkt — Default global key bindings
;;
;; init-global-keymap! populates the global keymap with
;; standard Emacs-like bindings.

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

  ;; ── C-x prefix keymap ──
  (define ctrl-x-map (make-keymap))
  (keymap-set! ctrl-x-map (key 'char #\o)       cmd-other-window)
  (keymap-set! ctrl-x-map (key 'char #\2)       cmd-split-window-below)
  (keymap-set! ctrl-x-map (key 'char #\3)       cmd-split-window-right)
  (keymap-set! ctrl-x-map (key 'char #\0)       cmd-delete-window)
  (keymap-set! ctrl-x-map (key 'char #\1)       cmd-delete-other-windows)
  (keymap-set! ctrl-x-map (key 'char #\^)       cmd-enlarge-window)
  (keymap-set! ctrl-x-map (key 'char #\{) cmd-shrink-window-horizontally)
  (keymap-set! ctrl-x-map (key 'char #\}) cmd-enlarge-window-horizontally)
  (keymap-set! ctrl-x-map (key 'char #\+)       cmd-balance-windows)
  (keymap-set! global-keymap (key 'ctrl #\x)    ctrl-x-map))
