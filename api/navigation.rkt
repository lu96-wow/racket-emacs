#lang racket

;; api/navigation.rkt — Window-level cursor movement commands
;;
;; Each command operates on the selected window: reading point from
;; the window's pointm (which is the buffer's point-marker for the
;; selected window), moving it, then re-centering the window.
;;
;; Text-level helpers (line-beginning, current-display-column, etc.)
;; are internal — only window-level commands are exported.

(require "../kernel/buffer.rkt"
         "../kernel/text.rkt"
         "../kernel/gap/gap.rkt"
         "../kernel/gap/query.rkt"
         "../display/char-width.rkt"
         "../display/window.rkt"
         "../display/render.rkt"
         "command.rkt")

(provide
 cmd-forward-char cmd-backward-char
 cmd-next-line cmd-prev-line
 cmd-beginning-of-line cmd-end-of-line)

;; ============================================================
;; Text-level helpers (internal)
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

(define (display-column buf pos)
  (define gb (text-gap (buffer-text buf)))
  (define bol (line-beginning gb pos))
  (gap-display-width gb bol pos))

(define (move-to-column gb bol target-col)
  (define len (gap-length gb))
  (define eol (line-end gb bol len))
  (scan-display-width gb bol eol target-col))

;; ============================================================
;; Window-level commands
;; ============================================================

(define-command cmd-forward-char "forward-char" (win frm evt)
  (define buf (window-buffer win))
  (define pt (buffer-point buf))
  (define gb (text-gap (buffer-text buf)))
  (when (< pt (buffer-length buf))
    (set-buffer-point! buf (gap-next-char-pos gb pt)))
  (recenter-point! win))

(define-command cmd-backward-char "backward-char" (win frm evt)
  (define buf (window-buffer win))
  (define pt (buffer-point buf))
  (when (> pt 0)
    (define gb (text-gap (buffer-text buf)))
    (set-buffer-point! buf (gap-prev-char-pos gb pt)))
  (recenter-point! win))

(define-command cmd-beginning-of-line "beginning-of-line" (win frm evt)
  (define buf (window-buffer win))
  (define gb (text-gap (buffer-text buf)))
  (define pt (buffer-point buf))
  (set-buffer-point! buf (line-beginning gb pt))
  (recenter-point! win))

(define-command cmd-end-of-line "end-of-line" (win frm evt)
  (define buf (window-buffer win))
  (define gb (text-gap (buffer-text buf)))
  (define pt (buffer-point buf))
  (define len (buffer-length buf))
  (set-buffer-point! buf (line-end gb pt len))
  (recenter-point! win))

(define-command cmd-next-line "next-line" (win frm evt)
  (define buf (window-buffer win))
  (define gb (text-gap (buffer-text buf)))
  (define pt (buffer-point buf))
  (define len (buffer-length buf))
  (define goal-col (display-column buf pt))
  (define eol (line-end gb pt len))
  (when (< eol len)
    (define next-bol (gap-next-char-pos gb eol))
    (set-buffer-point! buf (move-to-column gb next-bol goal-col)))
  (recenter-point! win))

(define-command cmd-prev-line "prev-line" (win frm evt)
  (define buf (window-buffer win))
  (define gb (text-gap (buffer-text buf)))
  (define pt (buffer-point buf))
  (define goal-col (display-column buf pt))
  (define bol (line-beginning gb pt))
  (if (> bol 0)
      (let* ([prev-end (gap-prev-char-pos gb bol)]
             [prev-bol (line-beginning gb prev-end)])
        (set-buffer-point! buf (move-to-column gb prev-bol goal-col)))
      (set-buffer-point! buf 0))
  (recenter-point! win))
