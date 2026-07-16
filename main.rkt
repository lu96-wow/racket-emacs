#lang racket

;; main.rkt — Editor entry point
;;
;; Thin composition: platform → api → kernel → display.
;; Event loop: read → dispatch → execute → commit-undo → redraw

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
         "api/bindings.rkt")

(define welcome-text
  (string-append
   ";; Welcome to racket-emacs-rebuild\n"
   ";; Clean functional window layer: calc → apply pipeline\n"
   ";;\n"
   ";; Keys:\n"
   ";;   C-f C-b C-n C-p    movement\n"
   ";;   C-a C-e            bol / eol\n"
   ";;   C-d C-k            delete / kill\n"
   ";;   C-y C-_            yank / undo\n"
   ";;   C-o                other window\n"
   ";;   C-2 C-3            split below / right\n"
   ";;   C-0 C-1            delete window / delete others\n"
   ";;   C-t                toggle wrap\n"
   ";;   C-q                quit\n"
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
    [(mouse-event? evt)
     (handle-mouse evt)
     ((unbox render-slot) frm)
     (event-loop)]

    [(and (key-event? evt)
          (key-event-ctrl? evt) (key-event-char evt)
          (char=? (char-downcase (key-event-char evt)) #\q))
     (void)]

    [else
     (define lf (selected-leaf))
     (define buf (and lf (leaf-buffer lf)))
     (define cmd (or (and buf (lookup-key buf evt))
                     (and (self-insert-key? evt) cmd-self-insert)))
     (when (command? cmd)
       ((command-fn cmd) lf frm evt)
       (define buf* (and lf (leaf-buffer lf)))
       (when (and buf* (command-modifies? cmd))
         (recorder-commit! (buffer-undo-recorder buf*))))
     ((unbox render-slot) frm)
     (event-loop)]))

;; ============================================================
;; Mouse handler
;; ============================================================

(define (handle-mouse evt)
  (define frm (current-frame))
  (define action (mouse-event-action evt))
  (when (and frm (eq? action 'press))
    (define-values (pos hit-lf hit-type)
      (screen-coord->buffer-pos frm (mouse-event-y evt) (mouse-event-x evt)))
    (when (and pos (eq? hit-type 'text))
      (when (and hit-lf (not (eq? hit-lf (frame-selected frm))))
        (set-frame-selected! frm hit-lf))
      (define hit-buf (leaf-buffer hit-lf))
      (set-buffer hit-buf)
      (set-buffer-point! hit-buf pos))))

;; ============================================================
;; Main
;; ============================================================

(define (main)
  (with-handlers ([exn:fail? (λ (e) (screen-cleanup!) (raise e))])
    (screen-init!)
    (detect-color-depth!)
    (format-alt-screen-enable)
    (display format-clear-screen)
    (display format-mouse-enable)
    (display format-bracketed-paste-disable)
    (flush-output)

    (init-global-keymap!)

    (define racket-km (make-keymap))
    (register-mode! (editor-mode 'racket racket-km ".rkt"))

    (define main-buf (get-buffer-create "*scratch*"))
    (buffer-insert! main-buf welcome-text 0)
    (set-buffer-point! main-buf 0)
    (init-buffer-with-filename! main-buf "*scratch*.rkt")

    (init-frame main-buf (terminal-width) (terminal-height))
    ((unbox render-slot) (current-frame))

    (with-handlers ([exn:break? (λ (e) (void))])
      (event-loop))

    (format-alt-screen-disable)
    (screen-cleanup!)
    (display "\n")))

(module+ main
  (main))
