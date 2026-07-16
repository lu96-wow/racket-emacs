#lang racket

;; api/command.rkt — Command protocol with dirty-flag integration
;;
;; Commands use (buf evt) signature — they modify buffer, not window.
;; Window is the event loop's concern (selected-leaf).
;; Modifying commands set the dirty flag for lazy redisplay.

(require "../display/dirty.rkt")

(provide
 command? command
 command-name command-fn command-modifies?
 define-command define-modify-command)

;; ============================================================
;; Struct
;; ============================================================

(struct command
  (name       ; string
   fn         ; (buffer key-event) -> void
   modifies?) ; boolean
  #:transparent)

;; ============================================================
;; define-command — non-modifying
;; ============================================================

(define-syntax-rule (define-command id name (buf evt) body ...)
  (define id
    (command name (λ (buf evt) body ...) #f)))

;; ============================================================
;; define-modify-command — buffer-modifying
;; ============================================================

(define-syntax-rule (define-modify-command id name (buf evt) body ...)
  (define id
    (command name (λ (buf evt)
                    body ...
                    (mark-redisplay-needed!))
             #t)))
