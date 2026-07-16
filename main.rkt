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
  (define prev
    (let loop ([p pt])
      (if (<= p 0) 0
          (let ([pp (gap-prev-char-pos gb p)])
            (if (char=? (gap-char gb pp) #\newline)
                (gap-next-char-pos gb pp)
                (loop pp))))))
  (set-buffer-point! buf prev))

(define (end-of-line buf)
  (define gb (text-gap (buffer-text buf)))
  (define pt (buffer-point buf))
  (define len (buffer-length buf))
  (define next
    (let loop ([p pt])
      (cond [(>= p len) len]
            [(char=? (gap-char gb p) #\newline) p]
            [else (loop (gap-next-char-pos gb p))])))
  (set-buffer-point! buf next))

(define (prev-line buf)
  (define gb (text-gap (buffer-text buf)))
  (define pt (buffer-point buf))
  (define bol
    (let loop ([p pt])
      (if (<= p 0) 0
          (let ([pp (gap-prev-char-pos gb p)])
            (if (char=? (gap-char gb pp) #\newline)
                (gap-next-char-pos gb pp)
                (loop pp))))))
  (if (> bol 0)
      (let* ([prev-end (gap-prev-char-pos gb bol)]
             [prev-bol
              (let loop ([p prev-end])
                (if (<= p 0) 0
                    (let ([pp (gap-prev-char-pos gb p)])
                      (if (char=? (gap-char gb pp) #\newline)
                          (gap-next-char-pos gb pp)
                          (loop pp)))))])
        (set-buffer-point! buf prev-bol))
      (set-buffer-point! buf 0)))

(define (next-line buf)
  (define gb (text-gap (buffer-text buf)))
  (define pt (buffer-point buf))
  (define len (buffer-length buf))
  (define eol
    (let loop ([p pt])
      (cond [(>= p len) len]
            [(char=? (gap-char gb p) #\newline) p]
            [else (loop (gap-next-char-pos gb p))])))
  (when (< eol len)
    (set-buffer-point! buf (gap-next-char-pos gb eol))))

;; ============================================================
;; Editing commands
;; ============================================================

(define (self-insert buf ke)
  (define ch (key-event-char ke))
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
  (define ke (read-key-event!))
  (define rec (buffer-undo-recorder buf))

  (cond
    ;; C-q → quit
    [(and (key-event-ctrl? ke) (key-event-char ke)
          (char=? (char-downcase (key-event-char ke)) #\q))
     (void)]

    ;; Self-insert
    [(self-insert-key? ke)
     (self-insert buf ke)]

    ;; Backspace
    [(backspace-key? ke)
     (backward-delete buf)]

    ;; Return
    [(return-key? ke)
     (cmd-newline buf)]

    ;; Tab
    [(and (key-event-char ke) (char=? (key-event-char ke) #\tab))
     (self-insert buf ke)]

    ;; C-f / right
    [(or (and (key-event-ctrl? ke) (key-event-char ke)
              (char=? (char-downcase (key-event-char ke)) #\f))
         (eq? (key-event-symbol ke) 'right))
     (forward-char buf)]

    ;; C-b / left
    [(or (and (key-event-ctrl? ke) (key-event-char ke)
              (char=? (char-downcase (key-event-char ke)) #\b))
         (eq? (key-event-symbol ke) 'left))
     (backward-char buf)]

    ;; C-a / home
    [(or (and (key-event-ctrl? ke) (key-event-char ke)
              (char=? (char-downcase (key-event-char ke)) #\a))
         (eq? (key-event-symbol ke) 'home))
     (beginning-of-line buf)]

    ;; C-e / end
    [(or (and (key-event-ctrl? ke) (key-event-char ke)
              (char=? (char-downcase (key-event-char ke)) #\e))
         (eq? (key-event-symbol ke) 'end))
     (end-of-line buf)]

    ;; C-p / up
    [(or (and (key-event-ctrl? ke) (key-event-char ke)
              (char=? (char-downcase (key-event-char ke)) #\p))
         (eq? (key-event-symbol ke) 'up))
     (prev-line buf)]

    ;; C-n / down
    [(or (and (key-event-ctrl? ke) (key-event-char ke)
              (char=? (char-downcase (key-event-char ke)) #\n))
         (eq? (key-event-symbol ke) 'down))
     (next-line buf)]

    ;; C-t: toggle wrap mode
    [(and (key-event-ctrl? ke) (key-event-char ke)
          (char=? (char-downcase (key-event-char ke)) #\t))
     (define new-mode (if (eq? (buffer-wrap-mode buf) (quote none)) (quote char) (quote none)))
     (set-buffer-wrap-mode! buf new-mode)
     (set-bottom-line-echo! (format "Wrap: ~a" new-mode))
     (invalidate-frame-cache!)]

    ;; C-d: delete forward
    [(and (key-event-ctrl? ke) (key-event-char ke)
          (char=? (char-downcase (key-event-char ke)) #\d))
     (forward-delete buf)]

    ;; C-k: kill line
    [(and (key-event-ctrl? ke) (key-event-char ke)
          (char=? (char-downcase (key-event-char ke)) #\k))
     (kill-line buf)]

    ;; C-y: yank
    [(and (key-event-ctrl? ke) (key-event-char ke)
          (char=? (char-downcase (key-event-char ke)) #\y))
     (yank buf)]

    ;; C-_ / C-/ : undo
    [(and (key-event-ctrl? ke) (key-event-char ke)
          (or (char=? (key-event-char ke) #\_)
              (char=? (key-event-char ke) #\/)))
     (buffer-undo! buf)]

    ;; C-r: redo
    [(and (key-event-ctrl? ke) (key-event-char ke)
          (char=? (char-downcase (key-event-char ke)) #\r))
     (buffer-redo! buf)]

    [else (void)])

  ;; Commit undo group
  (recorder-commit! rec)

  ;; Redraw via frame
  (display-frame (current-frame))

  ;; Continue unless quit
  (unless (and (key-event-ctrl? ke) (key-event-char ke)
               (char=? (char-downcase (key-event-char ke)) #\q))
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
    (display format-mouse-disable)
    (display format-bracketed-paste-disable)
    (flush-output)

    ;; Create buffers
    (define main-buf (get-buffer-create "*scratch*"))
    (buffer-insert! main-buf welcome-text 0)
    (set-buffer-point! main-buf 0)
    (set-buffer main-buf)
    (register-buffer! main-buf)

    (define mini-buf (get-buffer-create " *minibuf*"))
    (register-buffer! mini-buf)

    ;; Create frame
    (void (init-root-frame main-buf mini-buf (terminal-width) (terminal-height)))
    (display-frame (current-frame))

    (with-handlers ([exn:break? (λ (e) (void))])
      (event-loop main-buf))

    (format-alt-screen-disable)
    (screen-cleanup!)
    (display "\n")))

(module+ main
  (main))
