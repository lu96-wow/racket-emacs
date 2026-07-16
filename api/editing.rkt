#lang racket

;; api/editing.rkt — Window-level editing commands
;;
;; Buffer-modifying commands set modifies? = #t so the event-loop
;; commits a buffer undo boundary after execution.
;; Undo/redo delegate directly to buffer-undo!/buffer-redo!.

(require "../kernel/buffer.rkt"
         "../kernel/text.rkt"
         "../kernel/gap/query.rkt"
         "../kernel/key-event/key-event.rkt"
         "../display/window.rkt"
         "../display/render.rkt"
         "../display/layout.rkt"
         "command.rkt")

(provide
 cmd-self-insert cmd-newline cmd-tab
 cmd-backward-delete cmd-forward-delete
 cmd-kill-line cmd-yank
 cmd-undo cmd-redo
 cmd-toggle-wrap-mode
 kill-ring-contents)

;; ============================================================
;; Kill ring
;; ============================================================

(define kill-ring-contents (box ""))

;; ============================================================
;; Editing commands
;; ============================================================

(define-modify-command cmd-self-insert "self-insert" (win frm evt)
  (define ch (key-event-char evt))
  (when ch
    (define buf (window-buffer win))
    (buffer-insert! buf (string ch) (buffer-point buf))))

(define-modify-command cmd-newline "newline" (win frm evt)
  (define buf (window-buffer win))
  (define pt (buffer-point buf))
  (buffer-insert! buf "\n" pt)
  (set-buffer-point! buf (add1 pt)))

(define-modify-command cmd-tab "tab" (win frm evt)
  (define buf (window-buffer win))
  (buffer-insert! buf "\t" (buffer-point buf)))

(define-modify-command cmd-backward-delete "backward-delete" (win frm evt)
  (define buf (window-buffer win))
  (define pt (buffer-point buf))
  (when (> pt 0)
    (define gb (text-gap (buffer-text buf)))
    (define prev (gap-prev-char-pos gb pt))
    (buffer-delete! buf prev pt)
    (set-buffer-point! buf prev)))

(define-modify-command cmd-forward-delete "forward-delete" (win frm evt)
  (define buf (window-buffer win))
  (define pt (buffer-point buf))
  (when (< pt (buffer-length buf))
    (define gb (text-gap (buffer-text buf)))
    (define next (gap-next-char-pos gb pt))
    (buffer-delete! buf pt next)))

(define-modify-command cmd-kill-line "kill-line" (win frm evt)
  (define buf (window-buffer win))
  (define gb (text-gap (buffer-text buf)))
  (define pt (buffer-point buf))
  (define len (buffer-length buf))
  (define eol
    (let loop ([p pt])
      (cond [(>= p len) len]
            [(char=? (gap-char gb p) #\newline) p]
            [else (loop (gap-next-char-pos gb p))])))
  (cond [(= pt eol)
         (when (< pt len)
           (buffer-delete! buf pt (gap-next-char-pos gb pt))
           (set-box! kill-ring-contents "\n"))]
        [else
         (define text (buffer-substring buf pt eol))
         (buffer-delete! buf pt eol)
         (set-box! kill-ring-contents text)]))

(define-modify-command cmd-yank "yank" (win frm evt)
  (define buf (window-buffer win))
  (define text (unbox kill-ring-contents))
  (when (positive? (string-length text))
    (define pt (buffer-point buf))
    (buffer-insert! buf text pt)
    (set-buffer-point! buf (+ pt (bytes-length (string->bytes/utf-8 text))))))

(define-modify-command cmd-undo "undo" (win frm evt)
  (buffer-undo! (window-buffer win)))

(define-modify-command cmd-redo "redo" (win frm evt)
  (buffer-redo! (window-buffer win)))

;; ============================================================
;; Non-modifying
;; ============================================================

(define-command cmd-toggle-wrap-mode "toggle-wrap-mode" (win frm evt)
  (define buf (window-buffer win))
  (define new-mode (if (eq? (buffer-wrap-mode buf) 'none) 'char 'none))
  (set-buffer-wrap-mode! buf new-mode)
  (invalidate-frame-cache! frm))
