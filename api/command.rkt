#lang racket

;; api/command.rkt — Command protocol
;;
;; A command is a named operation.  It carries a `modifies?` flag
;; so the event-loop knows whether to manage undo boundaries.
;;
;; The event-loop is responsible for the full lifecycle:
;;   undo-boundary → execute → commit-undo → redraw
;;
;; Commands themselves are pure functions: (window frame key-event) -> void.
;; They don't touch undo or redraw — that's the loop's job.

(provide
 command? command
 command-name command-fn command-modifies?

 define-command
 define-modify-command)

;; ============================================================
;; Struct
;; ============================================================

(struct command
  (name       ; string — for display / debug
   fn         ; (window frame key-event) -> void
   modifies?) ; boolean — does this change buffer content?
  #:transparent)

;; ============================================================
;; define-command — non-modifying (navigation, window ops)
;; ============================================================

(define-syntax-rule (define-command id name (win frm evt) body ...)
  (define id
    (command name (λ (win frm evt) body ...) #f)))

;; ============================================================
;; define-modify-command — buffer-modifying (insert, delete, kill, yank, undo, redo)
;; ============================================================

(define-syntax-rule (define-modify-command id name (win frm evt) body ...)
  (define id
    (command name (λ (win frm evt) body ...) #t)))
