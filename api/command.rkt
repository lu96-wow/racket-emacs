#lang racket

;; api/command.rkt — Command protocol
;;
;; A command is a named operation with a `modifies?` flag.
;; The event-loop uses this flag to decide whether to commit
;; a buffer undo boundary after execution.
;;
;; Commands are pure functions (window frame key-event) -> void.
;; No undo-fn, no state-fn — undo is buffer-level only.

(provide
 command? command
 command-name command-fn command-modifies?
 define-command)

;; ============================================================
;; Struct
;; ============================================================

(struct command
  (name       ; string
   fn         ; (window frame key-event) -> void
   modifies?) ; boolean — does this change buffer content?
  #:transparent)

;; ============================================================
;; define-command — default modifies? = #f
;; ============================================================

(define-syntax-rule (define-command id name (win frm evt) body ...)
  (define id
    (command name (λ (win frm evt) body ...) #f)))
