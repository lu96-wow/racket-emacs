#lang racket

;; user/fundamental.rkt — Fundamental mode

(require "../kernel/keymap.rkt"
         "../kernel/buffer.rkt"
         "../kernel/syntax.rkt"
         "../base/edit.rkt"
         "../base/keybind.rkt"
         "../base/window-ops.rkt"
         "../base/registry.rkt"
         "../base/isearch.rkt"
         "mode.rkt")

(provide fundamental-keymap fundamental-mode init-fundamental-mode!)

(define fundamental-keymap (make-keymap))

(define (init-fundamental-bindings!)
  (bind-key fundamental-keymap "C-f" forward-char)    (bind-key fundamental-keymap "C-b" backward-char)
  (bind-key fundamental-keymap "C-n" forward-line)    (bind-key fundamental-keymap "C-p" backward-line)
  (bind-key fundamental-keymap "C-a" beginning-of-line) (bind-key fundamental-keymap "C-e" end-of-line)
  (bind-key fundamental-keymap "M-f" forward-word)    (bind-key fundamental-keymap "M-b" backward-word)
  (bind-key fundamental-keymap "M-<" beginning-of-buffer) (bind-key fundamental-keymap "M->" end-of-buffer)
  (bind-key fundamental-keymap "up" backward-line) (bind-key fundamental-keymap "down" forward-line)
  (bind-key fundamental-keymap "left" backward-char) (bind-key fundamental-keymap "right" forward-char)
  (bind-key fundamental-keymap "S-up" shift-backward-line) (bind-key fundamental-keymap "S-down" shift-forward-line)
  (bind-key fundamental-keymap "S-left" shift-backward-char) (bind-key fundamental-keymap "S-right" shift-forward-char)
  (bind-key fundamental-keymap "C-d" delete-char)     (bind-key fundamental-keymap "DEL" delete-backward-char)
  (bind-key fundamental-keymap "delete" delete-char)  (bind-key fundamental-keymap "C-k" kill-line)
  (bind-key fundamental-keymap "RET" newline)
  (bind-key fundamental-keymap "C-_" undo) (bind-key fundamental-keymap "C-x u" undo) (bind-key fundamental-keymap "C-x r" redo)
  (bind-key fundamental-keymap "C-y" yank) (bind-key fundamental-keymap "M-y" yank-pop)
  (bind-key fundamental-keymap "C-SPC" (λ () (set-mark)))
  (bind-key fundamental-keymap "C-x h" (λ () (mark-whole-buffer)))
  (bind-key fundamental-keymap "C-x C-x" (λ () (exchange-point-and-mark)))
  (bind-key fundamental-keymap "C-w" (λ () (kill-region)))
  (bind-key fundamental-keymap "C-s" (λ () (isearch-forward)))
  (bind-key fundamental-keymap "C-r" (λ () (isearch-backward)))
  (bind-key fundamental-keymap "C-x t" (λ () (toggle-truncate-lines))))

(define fundamental-mode
  (define-mode 'Fundamental
    #:keymap fundamental-keymap
    #:syntax (make-standard-syntax-table)
    #:file-types '("" ".txt")))

(define (init-fundamental-mode!)
  (init-fundamental-bindings!))
