#lang racket

;; main.rkt — Editor entry point
;;
;; Composes platform → kernel → display into a runnable editor.
;; Uses window/frame system from display/window.rkt.

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
         "display/char-width.rkt"
         "display/registry.rkt"
         "display/face.rkt")

;; ============================================================
;; Welcome text
;; ============================================================

(define welcome-text
  (string-append
   ";; Welcome to racket-emacs-rebuild\n"
   ";; kernel + platform + display layered architecture\n"
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
   ";;   C-q              quit\n"
   ";;\n\n"
   "(define (hello)\n"
   "  (displayln \"你好, world!\"))\n"))

;; ============================================================
;; Navigation
;; ============================================================

(define (forward-char buf)
  (define pt (buffer-point buf))
  (define gb (text-gap (buffer-text buf)))
  (when (< pt (buffer-length buf))
    (set-buffer-point! buf (gap-next-char-pos gb pt))))

(define (backward-char buf)
  (define pt (buffer-point buf))
  (when (> pt 0)
    (define gb (text-gap (buffer-text buf)))
    (set-buffer-point! buf (gap-prev-char-pos gb pt))))

(define (beginning-of-line buf)
  (define gb (text-gap (buffer-text buf)))
  (define pt (buffer-point buf))
  (set-buffer-point! buf (line-beginning gb pt)))

(define (end-of-line buf)
  (define gb (text-gap (buffer-text buf)))
  (define pt (buffer-point buf))
  (define len (buffer-length buf))
  (set-buffer-point! buf (line-end gb pt len)))

(define (prev-line buf)
  (define gb (text-gap (buffer-text buf)))
  (define pt (buffer-point buf))
  ;; Record goal column
  (define goal-col (current-display-column buf pt))
  ;; Find bol of current line
  (define bol (line-beginning gb pt))
  (if (> bol 0)
      (let* ([prev-end (gap-prev-char-pos gb bol)]
             [prev-bol (line-beginning gb prev-end)])
        (set-buffer-point! buf (move-to-display-column gb prev-bol goal-col)))
      (set-buffer-point! buf 0)))

(define (next-line buf)
  (define gb (text-gap (buffer-text buf)))
  (define pt (buffer-point buf))
  (define len (buffer-length buf))
  ;; Record goal column
  (define goal-col (current-display-column buf pt))
  ;; Find eol of current line
  (define eol (line-end gb pt len))
  (when (< eol len)
    (define next-bol (gap-next-char-pos gb eol))
    (set-buffer-point! buf (move-to-display-column gb next-bol goal-col))))

;; ============================================================
;; Line / column helpers
;; ============================================================

(define (line-beginning gb pos)
  (let loop ([p pos])
    (if (<= p 0) 0
        (let ([pp (gap-prev-char-pos gb p)])
          (if (char=? (gap-char gb pp) #\newline)
              (gap-next-char-pos gb pp)
              (loop pp))))))

(define (line-end gb pos len)
  (let loop ([p pos])
    (cond [(>= p len) len]
          [(char=? (gap-char gb p) #\newline) p]
          [else (loop (gap-next-char-pos gb p))])))

(define (current-display-column buf pos)
  (define gb (text-gap (buffer-text buf)))
  (define bol (line-beginning gb pos))
  (gap-display-width gb bol pos))

(define (move-to-display-column gb bol target-col)
  (define len (gap-length gb))
  (define eol (line-end gb bol len))
  (scan-display-width gb bol eol target-col))

;; ============================================================
;; Editing commands
;; ============================================================

(define (self-insert buf evt)
  (define ch (key-event-char evt))
  (when ch
    (buffer-insert! buf (string ch) (buffer-point buf))))

(define (backward-delete buf)
  (define pt (buffer-point buf))
  (when (> pt 0)
    (define gb (text-gap (buffer-text buf)))
    (define prev (gap-prev-char-pos gb pt))
    (buffer-delete! buf prev pt)
    (set-buffer-point! buf prev)))

(define (forward-delete buf)
  (define pt (buffer-point buf))
  (when (< pt (buffer-length buf))
    (define gb (text-gap (buffer-text buf)))
    (define next (gap-next-char-pos gb pt))
    (buffer-delete! buf pt next)))

(define (cmd-newline buf)
  (define pt (buffer-point buf))
  (buffer-insert! buf "\n" pt)
  (set-buffer-point! buf (add1 pt)))

;; ============================================================
;; Kill / Yank
;; ============================================================

(define kill-storage (box ""))

(define (kill-line buf)
  (define gb (text-gap (buffer-text buf)))
  (define pt (buffer-point buf))
  (define len (buffer-length buf))
  (define eol
    (let loop ([p pt])
      (cond [(>= p len) len]
            [(char=? (gap-char gb p) #\newline) p]
            [else (loop (gap-next-char-pos gb p))])))
  (cond
    [(= pt eol)
     (when (< pt len)
       (buffer-delete! buf pt (gap-next-char-pos gb pt))
       (set-box! kill-storage "\n"))]
    [else
     (define text (buffer-substring buf pt eol))
     (buffer-delete! buf pt eol)
     (set-box! kill-storage text)]))

(define (yank buf)
  (define text (unbox kill-storage))
  (when (positive? (string-length text))
    (define pt (buffer-point buf))
    (buffer-insert! buf text pt)
    (set-buffer-point! buf (+ pt (bytes-length (string->bytes/utf-8 text))))))

;; ============================================================
;; Event loop
;; ============================================================

(define (event-loop buf)
  (define evt (read-key-event!))
  (define rec (buffer-undo-recorder buf))

  (cond
    ;; Mouse event — set point at click position (never leaks to key processing)
    [(mouse-event? evt)
     (define frm (current-frame))
     (define action (mouse-event-action evt))
     (when (and frm (eq? action 'press))
       (define-values (pos hit-win hit-type)
         (screen-coord->buffer-pos frm (mouse-event-y evt) (mouse-event-x evt)))
       (when (and pos (eq? hit-type 'text))
         (set-buffer-point! buf pos)
         ;; Switch buffer if clicked on a different buffer's window
         (when (and hit-win (window-buffer hit-win)
                    (not (eq? (window-buffer hit-win) buf)))
           (set-buffer (window-buffer hit-win))
           (set! buf (window-buffer hit-win)))))]

    ;; C-q → quit
    [(and (key-event-ctrl? evt) (key-event-char evt)
          (char=? (char-downcase (key-event-char evt)) #\q))
     (void)]

    ;; Self-insert
    [(self-insert-key? evt)
     (self-insert buf evt)]

    ;; Backspace
    [(backspace-key? evt)
     (backward-delete buf)]

    ;; Return
    [(return-key? evt)
     (cmd-newline buf)]

    ;; Tab
    [(and (key-event-char evt) (char=? (key-event-char evt) #\tab))
     (self-insert buf evt)]

    ;; C-f / right
    [(or (and (key-event-ctrl? evt) (key-event-char evt)
              (char=? (char-downcase (key-event-char evt)) #\f))
         (eq? (key-event-symbol evt) 'right))
     (forward-char buf)]

    ;; C-b / left
    [(or (and (key-event-ctrl? evt) (key-event-char evt)
              (char=? (char-downcase (key-event-char evt)) #\b))
         (eq? (key-event-symbol evt) 'left))
     (backward-char buf)]

    ;; C-a / home
    [(or (and (key-event-ctrl? evt) (key-event-char evt)
              (char=? (char-downcase (key-event-char evt)) #\a))
         (eq? (key-event-symbol evt) 'home))
     (beginning-of-line buf)]

    ;; C-e / end
    [(or (and (key-event-ctrl? evt) (key-event-char evt)
              (char=? (char-downcase (key-event-char evt)) #\e))
         (eq? (key-event-symbol evt) 'end))
     (end-of-line buf)]

    ;; C-p / up
    [(or (and (key-event-ctrl? evt) (key-event-char evt)
              (char=? (char-downcase (key-event-char evt)) #\p))
         (eq? (key-event-symbol evt) 'up))
     (prev-line buf)]

    ;; C-n / down
    [(or (and (key-event-ctrl? evt) (key-event-char evt)
              (char=? (char-downcase (key-event-char evt)) #\n))
         (eq? (key-event-symbol evt) 'down))
     (next-line buf)]

    ;; C-t: toggle wrap mode
    [(and (key-event-ctrl? evt) (key-event-char evt)
          (char=? (char-downcase (key-event-char evt)) #\t))
     (define new-mode (if (eq? (buffer-wrap-mode buf) (quote none)) (quote char) (quote none)))
     (set-buffer-wrap-mode! buf new-mode)
     ;; (no minibuffer yet) (void)
     (invalidate-frame-cache!)]

    ;; C-d: delete forward
    [(and (key-event-ctrl? evt) (key-event-char evt)
          (char=? (char-downcase (key-event-char evt)) #\d))
     (forward-delete buf)]

    ;; C-k: kill line
    [(and (key-event-ctrl? evt) (key-event-char evt)
          (char=? (char-downcase (key-event-char evt)) #\k))
     (kill-line buf)]

    ;; C-y: yank
    [(and (key-event-ctrl? evt) (key-event-char evt)
          (char=? (char-downcase (key-event-char evt)) #\y))
     (yank buf)]

    ;; C-_ / C-/ : undo
    [(and (key-event-ctrl? evt) (key-event-char evt)
          (or (char=? (key-event-char evt) #\_)
              (char=? (key-event-char evt) #\/)))
     (buffer-undo! buf)]

    ;; C-r: redo
    [(and (key-event-ctrl? evt) (key-event-char evt)
          (char=? (char-downcase (key-event-char evt)) #\r))
     (buffer-redo! buf)]

    [else (void)])

  ;; Commit undo group
  (recorder-commit! rec)

  ;; Redraw via frame
  ((unbox render-slot) (current-frame))

  ;; Continue unless quit (mouse events don't quit)
  (unless (and (key-event? evt)
               (key-event-ctrl? evt) (key-event-char evt)
               (char=? (char-downcase (key-event-char evt)) #\q))
    (event-loop buf)))

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

    ;; Create buffers
    (define main-buf (get-buffer-create "*scratch*"))
    (buffer-insert! main-buf welcome-text 0)
    (set-buffer-point! main-buf 0)
    (set-buffer main-buf)
    (register-buffer! main-buf)

    ;; Create frame
    (void (init-root-frame main-buf (terminal-width) (terminal-height)))
    ((unbox render-slot) (current-frame))

    (with-handlers ([exn:break? (λ (e) (void))])
      (event-loop main-buf))

    (format-alt-screen-disable)
    (screen-cleanup!)
    (display "\n")))

(module+ main
  (main))
