#lang racket

;; main.rkt — Editor entry point
;;
;; Thin composition layer: platform → api → kernel → display.
;; The event-loop manages the full command lifecycle:
;;   undo-boundary → execute → commit-undo → redraw

(require "kernel/buffer.rkt"
         "kernel/text.rkt"
         "kernel/gap/gap.rkt"
         "kernel/gap/query.rkt"
         "kernel/key-event/key-event.rkt"
         "kernel/undo/recorder.rkt"
         "platform/ansi.rkt"
         "platform/termios.rkt"
         "platform/event.rkt"
         "display/render.rkt"
         "display/window.rkt"
         "display/registry.rkt"
         "display/face.rkt"
         "api/command.rkt"
         "api/editing.rkt"
         "api/keymap.rkt"
         "api/mode.rkt"
         "api/bindings.rkt"
         "api/display-buffer.rkt")

;; ============================================================
;; Welcome text
;; ============================================================

(define welcome-text
  (string-append
   ";; Welcome to racket-emacs-rebuild\n"
   ";; kernel + api + display layered architecture\n"
   ";;\n"
   ";; Keys:\n"
   ";;   Type to insert   Backspace to delete\n"
   ";;   C-p C-n          prev / next line\n"
   ";;   C-f C-b          forward / backward char\n"
   ";;   C-a C-e          bol / eol\n"
   ";;   C-d              delete forward\n"
   ";;   C-k              kill line\n"
   ";;   C-y              yank\n"
   ";;   C-_              undo\n"
   ";;   C-x o            other window\n"
   ";;   C-q              quit\n"
   ";;\n\n"
   "(define (hello)\n"
   "  (displayln \"你好, world!\"))\n"))

;; ============================================================
;; Event loop
;; ============================================================

(define (event-loop)
  (define evt (read-key-event!))
  (define frm (current-frame))

  (cond
    ;; ── Mouse ──
    [(mouse-event? evt)
     (handle-mouse evt)
     ((unbox render-slot) frm)
     (event-loop)]

    ;; ── Quit: C-q ──
    [(and (key-event? evt)
          (key-event-ctrl? evt) (key-event-char evt)
          (char=? (char-downcase (key-event-char evt)) #\q))
     (void)]

    [else
     ;; ── Dispatch with prefix-key support ──
     (define win (selected-window))
     (define buf (and win (window-buffer win)))
     (define initial (or (and buf (lookup-key buf evt))
                         (and (self-insert-key? evt) cmd-self-insert)))
     (cond
       [(keymap-value-keymap? initial)
        ;; Prefix key — read next key from this sub-keymap
        (define evt2 (read-key-event!))
        (define cmd (or (and buf (keymap-lookup initial (key-event->key evt2)))
                        (and (self-insert-key? evt2) cmd-self-insert)))
        (when (and cmd (command? cmd))
          ((command-fn cmd) win frm evt2)
          (when (command-modifies? cmd)
            (recorder-commit! (buffer-undo-recorder (window-buffer win)))))]
       [(command? initial)
        (when initial
          ((command-fn initial) win frm evt)
          (when (command-modifies? initial)
            (recorder-commit! (buffer-undo-recorder (window-buffer win)))))]
       [else (void)])
     ((unbox render-slot) frm)
     (event-loop)]))

;; ============================================================
;; Mouse handler (internal to event-loop)
;; ============================================================

(define (handle-mouse evt)
  (define frm (current-frame))
  (define action (mouse-event-action evt))
  (when (and frm (eq? action 'press))
    (define-values (pos hit-win hit-type)
      (screen-coord->buffer-pos frm (mouse-event-y evt) (mouse-event-x evt)))
    (when (and pos (eq? hit-type 'text))
      ;; Switch window if clicked on a different one
      (when (and hit-win (not (eq? hit-win (selected-window))))
        (set-window-selected?! (selected-window) #f)
        (set-window-selected?! hit-win #t)
        (set-frame-selected-window! frm hit-win))
      ;; Set point
      (define hit-buf (window-buffer hit-win))
      (set-buffer hit-buf)
      (set-buffer-point! hit-buf pos))))

;; ============================================================
;; Main
;; ============================================================

(define (main)
  (with-handlers ([exn:fail? (λ (e)
                    (screen-cleanup!)
                    (raise e))])
    (screen-init!)
    (detect-color-depth!)
    (format-alt-screen-enable)
    (display format-clear-screen)
    (display format-mouse-enable)
    (display format-bracketed-paste-disable)
    (flush-output)

    (init-global-keymap!)

    ;; Register modes (pattern → keymap)
    (define racket-km (make-keymap))
    (register-mode! (editor-mode 'racket racket-km ".rkt"))

    (define main-buf (get-buffer-create "*scratch*"))
    (buffer-insert! main-buf welcome-text 0)
    (set-buffer-point! main-buf 0)
    ;; Give scratch buffer a racket filename so racket mode applies
    (init-buffer-with-filename! main-buf "*scratch*.rkt")

    ;; Use display-buffer for initial window (creates frame if needed)
    (display-buffer main-buf #:action display-buffer-same-window)
    ((unbox render-slot) (current-frame))

    (with-handlers ([exn:break? (λ (e) (void))])
      (event-loop))

    (format-alt-screen-disable)
    (screen-cleanup!)
    (display "\n")))

(module+ main
  (main))
