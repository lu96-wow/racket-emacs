#lang racket

;; main.rkt — Editor entry point with lazy redisplay
;;
;; Architecture:
;;   read event → dispatch to command (buf evt) → mark dirty
;;   → at safe point: check dirty flag → render
;;
;; Separation of concerns:
;;   - Commands modify buffer, set dirty flag
;;   - Event loop owns window selection, undo, fontify
;;   - Render pipeline is called lazily, not after every event

(require "kernel/buffer.rkt"
         "kernel/text.rkt"
         "kernel/gap/gap.rkt"
         "kernel/gap/query.rkt"
         "kernel/undo/recorder.rkt"
         "platform/ansi.rkt"
         "platform/termios.rkt"
         "platform/event.rkt"
         "display/render.rkt"
         "display/window.rkt"
         "display/face.rkt"
         "display/registry.rkt"
         "display/mouse.rkt"
         "api/command.rkt"
         "api/editing.rkt"
         "api/keymap.rkt"
         "api/mode.rkt"
         "api/bindings.rkt"
         "base/font-lock.rkt"
         "api/lang.rkt"
         "api/lang/racket-lang.rkt")

(define welcome-text
  (string-append
   ";; Welcome to racket-emacs-rebuild-display\n"
   ";; Lazy redisplay + separated concerns\n"
   ";;\n"
   ";; Keys:\n"
   ";;   C-f C-b C-n C-p    movement\n"
   ";;   C-a C-e            bol / eol\n"
   ";;   C-d C-k            delete / kill\n"
   ";;   C-y M-y C-_        yank / yank-pop / undo\n"
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
  ;; ---- Safe point: render if needed ----
  (when (redisplay-needed?)
    ((unbox render-slot) (current-frame))
    (clear-redisplay-needed!))

  ;; ---- Read next event ----
  (define evt (read-key-event!))
  (define frm (current-frame))

  (cond
    ;; Mouse
    [(mouse-event? evt)
     (handle-mouse evt)
     (event-loop)]

    ;; Quit
    [(and (key-event? evt)
          (key-event-ctrl? evt) (key-event-char evt)
          (char=? (char-downcase (key-event-char evt)) #\q))
     (void)]

    ;; Keyboard → command dispatch
    [else
     (define buf (current-buffer))
     (define cmd (or (and buf (lookup-key buf evt))
                     (and (self-insert-key? evt) cmd-self-insert)))
     (when (command? cmd)
       ;; Execute command
       ((command-fn cmd) buf evt)

       ;; Post-command hooks (explicit, no implicit hook system)
       (when (command-modifies? cmd)
         ;; All commands get their buffer from global state, so buf is current
         (define actual-buf (current-buffer))
         (recorder-commit! (buffer-undo-recorder actual-buf))
         (fontify-after-change! actual-buf)
         (clear-buffer-change-region! actual-buf)))
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
      (set-buffer-point! hit-buf pos))
    (mark-redisplay-needed!)))

;; ============================================================
;; Main
;; ============================================================

(define (main)
  (with-handlers ([exn:fail? (λ (e) (screen-cleanup!) (raise e))])
    (screen-init!)
    (detect-color-depth!)
    (init-face-cache!)
    (format-alt-screen-enable)
    (display format-clear-screen)
    (display format-mouse-enable)
    (display format-bracketed-paste-disable)
    (flush-output)

    (init-global-keymap!)

    (define racket-km (make-keymap))
    (register-lang! racket-lang-config)

    (define main-buf (get-buffer-create "*scratch*"))
    (buffer-insert! main-buf welcome-text 0)
    (set-buffer-point! main-buf 0)
    (init-buffer-with-filename! main-buf "*scratch*.rkt")

    (init-frame main-buf (terminal-width) (terminal-height))
    (check-min-size!)

    ;; Initial render
    (mark-redisplay-needed!)
    ((unbox render-slot) (current-frame))

    (with-handlers ([exn:break? (λ (e) (void))])
      (event-loop))

    (format-alt-screen-disable)
    (screen-cleanup!)
    (display "\n")))

(module+ main
  (main))
