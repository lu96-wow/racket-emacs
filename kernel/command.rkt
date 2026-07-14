#lang racket

;; core/command.rkt — Command registry (pure data, zero IO)
;;
;; Maps command names (strings) to procedures.
;; Used by M-x and for future tab-completion.
;; No display or IO dependencies.

(provide
 define-command
 lookup-command
 command-names)

;; ============================================================
;; Command table — global, mutable
;; ============================================================

(define command-table (make-hash))  ; string → procedure

(define (define-command name proc)
  (hash-set! command-table name proc))

(define (lookup-command name)
  (hash-ref command-table name #f))

(define (command-names)
  (sort (hash-keys command-table) string<?))
