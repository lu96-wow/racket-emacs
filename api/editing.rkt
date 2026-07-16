#lang racket

;; api/editing.rkt — Buffer-modifying commands

(require "../kernel/buffer.rkt"
         "../kernel/text.rkt"
         "../kernel/gap/query.rkt"
         "../kernel/key-event/key-event.rkt"
         "../kernel/kill-ring/kill-ring.rkt"
         "../display/window.rkt"
         "../display/render.rkt"
         "../display/layout.rkt"
         "command.rkt")

(provide
 cmd-self-insert cmd-newline cmd-tab
 cmd-backward-delete cmd-forward-delete
 cmd-kill-line cmd-yank cmd-yank-pop
 cmd-undo cmd-redo
 cmd-toggle-wrap-mode)

(define-modify-command cmd-self-insert "self-insert" (lf frm evt)
  (define ch (key-event-char evt))
  (when ch
    (define buf (leaf-buffer lf))
    (buffer-insert! buf (string ch) (buffer-point buf))))

(define-modify-command cmd-newline "newline" (lf frm evt)
  (define buf (leaf-buffer lf))
  (define pt (buffer-point buf))
  (buffer-insert! buf "\n" pt)
  (set-buffer-point! buf (add1 pt)))

(define-modify-command cmd-tab "tab" (lf frm evt)
  (define buf (leaf-buffer lf))
  (buffer-insert! buf "\t" (buffer-point buf)))

(define-modify-command cmd-backward-delete "backward-delete" (lf frm evt)
  (define buf (leaf-buffer lf))
  (define pt (buffer-point buf))
  (when (> pt 0)
    (define gb (text-gap (buffer-text buf)))
    (define prev (gap-prev-char-pos gb pt))
    (buffer-delete! buf prev pt)
    (set-buffer-point! buf prev)))

(define-modify-command cmd-forward-delete "forward-delete" (lf frm evt)
  (define buf (leaf-buffer lf))
  (define pt (buffer-point buf))
  (when (< pt (buffer-length buf))
    (define gb (text-gap (buffer-text buf)))
    (define next (gap-next-char-pos gb pt))
    (buffer-delete! buf pt next)))

;; ============================================================
;; Yank state — module-level, shared between yank / yank-pop
;; ============================================================

(define yank-start-pos (box #f))
(define yank-end-pos   (box #f))
(define last-was-yank? (box #f))

;; ============================================================
;; kill-line
;; ============================================================

(define-modify-command cmd-kill-line "kill-line" (lf frm evt)
  (define buf (leaf-buffer lf))
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
           (kill-new "\n"))]
        [else
         (define text (buffer-substring buf pt eol))
         (buffer-delete! buf pt eol)
         (kill-new text)])
  (set-box! last-was-yank? #f))

;; ============================================================
;; yank
;; ============================================================

(define-modify-command cmd-yank "yank" (lf frm evt)
  (define buf (leaf-buffer lf))
  (define text (kill-ring-yank))
  (when (and text (positive? (string-length text)))
    (define pt (buffer-point buf))
    (set-box! yank-start-pos pt)
    (buffer-insert! buf text pt)
    (define new-pt (+ pt (bytes-length (string->bytes/utf-8 text))))
    (set-buffer-point! buf new-pt)
    (set-box! yank-end-pos new-pt)
    (set-box! last-was-yank? #t)))

;; ============================================================
;; yank-pop — M-y: replace last yanked text with previous kill
;; ============================================================

(define-modify-command cmd-yank-pop "yank-pop" (lf frm evt)
  (unless (unbox last-was-yank?)
    (error 'yank-pop "previous command was not a yank"))
  (define buf (leaf-buffer lf))
  (define prev-start (unbox yank-start-pos))
  (define prev-end   (unbox yank-end-pos))
  ;; Delete the previously yanked text
  (when (and prev-start prev-end (> prev-end prev-start))
    (buffer-delete! buf prev-start prev-end))
  ;; Rotate and insert previous kill
  (define text (or (kill-ring-pop)
                   (current-kill)
                   ""))
  (when (positive? (string-length text))
    (buffer-insert! buf text prev-start)
    (define new-end (+ prev-start (bytes-length (string->bytes/utf-8 text))))
    (set-buffer-point! buf new-end)
    (set-box! yank-end-pos new-end)))

(define-modify-command cmd-undo "undo" (lf frm evt)
  (buffer-undo! (leaf-buffer lf)))

(define-modify-command cmd-redo "redo" (lf frm evt)
  (buffer-redo! (leaf-buffer lf)))

(define-command cmd-toggle-wrap-mode "toggle-wrap-mode" (lf frm evt)
  (define buf (leaf-buffer lf))
  (define new-mode (if (eq? (buffer-wrap-mode buf) 'none) 'char 'none))
  (set-buffer-wrap-mode! buf new-mode)
  (invalidate-frame-cache! frm))
