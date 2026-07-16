#lang racket

;; api/editing.rkt — Window-level editing commands
;;
;; Buffer-modifying commands use define-modify-command, which
;; tells the event-loop to manage undo boundaries around execution.
;; Navigation-only commands in navigation.rkt use define-command.

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
;; Helper: make a buffer-modifying command
;; ============================================================
;; Undo delegates to buffer-undo! — the buffer's own undo ring
;; handles the reversal.  No state capture needed.

(define (modify-command name exec-fn)
  (command name
    exec-fn
    ;; undo — delegate to buffer-level undo
    (λ (win frm _state)
      (buffer-undo! (window-buffer win)))
    #f))

;; ============================================================
;; Command-level undo/redo — operates on command history
;; ============================================================

(define-no-undo-command cmd-undo "undo" (win frm evt)
  (define entry (command-history-pop!))
  (when entry
    (define cmd (car entry))
    (define state (cdr entry))
    ;; Push to redo before undoing
    (when (command-state-fn cmd)
      (command-redo-push! cmd ((command-state-fn cmd) win frm)))
    ((command-undo-fn cmd) win frm state)))

(define-no-undo-command cmd-redo "redo" (win frm evt)
  (define entry (command-redo-pop!))
  (when entry
    (define cmd (car entry))
    ;; Snapshot for undo chain
    (define state (and (command-state-fn cmd)
                       ((command-state-fn cmd) win frm)))
    ;; Re-execute the command
    ((command-fn cmd) win frm evt)
    ;; Push back to undo history
    (command-history-push! cmd state)))

;; ============================================================
;; Kill ring — shared string storage
;; ============================================================

(define kill-ring-contents (box ""))

;; ============================================================
;; Buffer-modifying commands
;; ============================================================

(define cmd-self-insert
  (modify-command "self-insert"
    (λ (win frm evt)
      (define ch (key-event-char evt))
      (when ch
        (define buf (window-buffer win))
        (buffer-insert! buf (string ch) (buffer-point buf))))))

(define cmd-newline
  (modify-command "newline"
    (λ (win frm evt)
      (define buf (window-buffer win))
      (define pt (buffer-point buf))
      (buffer-insert! buf "\n" pt)
      (set-buffer-point! buf (add1 pt)))))

(define cmd-tab
  (modify-command "tab"
    (λ (win frm evt)
      (define buf (window-buffer win))
      (buffer-insert! buf "\t" (buffer-point buf)))))

(define cmd-backward-delete
  (modify-command "backward-delete"
    (λ (win frm evt)
      (define buf (window-buffer win))
      (define pt (buffer-point buf))
      (when (> pt 0)
        (define gb (text-gap (buffer-text buf)))
        (define prev (gap-prev-char-pos gb pt))
        (buffer-delete! buf prev pt)
        (set-buffer-point! buf prev)))))

(define cmd-forward-delete
  (modify-command "forward-delete"
    (λ (win frm evt)
      (define buf (window-buffer win))
      (define pt (buffer-point buf))
      (when (< pt (buffer-length buf))
        (define gb (text-gap (buffer-text buf)))
        (define next (gap-next-char-pos gb pt))
        (buffer-delete! buf pt next)))))

(define cmd-kill-line
  (modify-command "kill-line"
    (λ (win frm evt)
      (define buf (window-buffer win))
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
           (set-box! kill-ring-contents "\n"))]
        [else
         (define text (buffer-substring buf pt eol))
         (buffer-delete! buf pt eol)
         (set-box! kill-ring-contents text)]))))

(define cmd-yank
  (modify-command "yank"
    (λ (win frm evt)
      (define buf (window-buffer win))
      (define text (unbox kill-ring-contents))
      (when (positive? (string-length text))
        (define pt (buffer-point buf))
        (buffer-insert! buf text pt)
        (set-buffer-point! buf (+ pt (bytes-length (string->bytes/utf-8 text))))))))

;; ============================================================
;; Non-modifying command (buffer setting, not content change)
;; ============================================================

(define-no-undo-command cmd-toggle-wrap-mode "toggle-wrap-mode" (win frm evt)
  (define buf (window-buffer win))
  (define new-mode (if (eq? (buffer-wrap-mode buf) 'none) 'char 'none))
  (set-buffer-wrap-mode! buf new-mode)
  (invalidate-frame-cache! frm))
