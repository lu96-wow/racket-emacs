#lang racket

;; main.rkt — Editor entry point

(require "kernel/buffer.rkt"
         "kernel/keymap.rkt"
         "kernel/window.rkt"
         "kernel/textprop.rkt"
         "kernel/bottom-input.rkt"
         "base/registry.rkt"
         "platform/ansi.rkt"
         "platform/termios.rkt"
         "platform/event.rkt"
         "display/face.rkt"
         "user/mode.rkt"
         "user/fundamental.rkt"
         "user/racket.rkt"
         "user/command-loop.rkt"
         "user/global-bindings.rkt"
         "user/font-lock-activate.rkt"
         "user/racket-complete.rkt")

(module+ main
  (current-completion-echo bottom-line-set-echo!)
  (unless (terminal?)
    (displayln "This editor requires a real terminal (TTY).")
    (displayln "Run directly in a terminal emulator, not a pipe or IDE.")
    (exit 1)))

(define input-decode-map (make-keymap))
(void (init-input-decode-map! input-decode-map
  (λ (km seq ke) (define-key km seq ke))))

(define (main)
  (with-handlers ([exn:fail? (λ (e)
                    (screen-cleanup!)
                    (displayln (exn-message e))
                    (exit 1))])
    (screen-init!)
    (format-alt-screen-enable)
    (display format-mouse-enable)
    (display format-bracketed-paste-enable)
    (detect-color-depth!)
    (init-face-cache!)
    (flush-output)

    (init-global-bindings!)
    (init-minibuffer-bindings!)

    (define main-buf (get-buffer-create "*scratch*"))
    (define initial-text
      (string-append ";; Welcome to racket-emacs-rebuild\n"
                     ";; kernel/base/user layered architecture\n\n"
                     "(define (hello)\n  (displayln \"你好, world!\"))\n"))
    (buffer-insert main-buf initial-text #:at 0)
    (set-buffer-point! main-buf 0)
    (init-buffer-text-properties! main-buf)
    (set-buffer main-buf)
    (setup-buffer-mode! main-buf 'Racket)

    (define mini-buf (get-buffer-create " *minibuf*" #:inhibit-hooks? #t))
    (void (init-root-frame main-buf mini-buf (terminal-width) (terminal-height)))

    (with-handlers ([exn:break? (λ (e) (void))])
      (command-loop input-decode-map lookup-key))
    (screen-cleanup!)))

(module+ main (main))
