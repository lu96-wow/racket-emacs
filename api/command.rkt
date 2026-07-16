#lang racket

;; api/command.rkt — Command protocol
;;
;; A command is a named operation with a `modifies?` flag.
;; The event-loop uses this flag to decide whether to commit
;; a buffer undo boundary after execution.
;;
;; Two constructors:
;;   define-command         — modifies? = #f (navigation, window)
;;   define-modify-command  — modifies? = #t (editing, undo/redo)

(provide
 command? command
 command-name command-fn command-modifies?
 define-command define-modify-command)

;; ============================================================
;; Struct
;; ============================================================

(struct command
  (name       ; string
   fn         ; (leaf frame key-event) -> void
   modifies?) ; boolean — does this change buffer content?
  #:transparent)

;; ============================================================
;; define-command — non-modifying (navigation, window ops)
;; ============================================================

(define-syntax-rule (define-command id name (win frm evt) body ...)
  (define id
    (command name (λ (win frm evt) body ...) #f)))

;; ============================================================
;; define-modify-command — buffer-modifying (editing, undo/redo)
;; ============================================================

(define-syntax-rule (define-modify-command id name (win frm evt) body ...)
  (define id
    (command name (λ (win frm evt) body ...) #t)))
