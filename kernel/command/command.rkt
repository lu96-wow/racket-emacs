#lang racket

;; kernel/command/command.rkt — Command registry (pure data, zero IO)
;;
;; A command-table maps command names (strings) to procedures.
;; Multiple tables compose: later tables override earlier ones.
;; current-command-table is a parameter for per-buffer/per-context setup.

(provide
 ;; types
 command-table? make-command-table
 command-table-map

 ;; composition
 compose-command-tables
 ;; global parameter
 current-command-table
 ;; operations
 define-command! lookup-command command-names)

;; ============================================================
;; Command table
;; ============================================================

(struct command-table
  ([map #:mutable])  ; (hash/c string? procedure?)
  #:transparent)

(define (make-command-table)
  (command-table (make-hash)))

;; ============================================================
;; Composition — later tables override earlier ones
;; ============================================================

(define (compose-command-tables . tables)
  (define result (make-command-table))
  (for ([t (in-list tables)])
    (for ([(k v) (in-hash (command-table-map t))])
      (hash-set! (command-table-map result) k v)))
  result)

;; ============================================================
;; Global parameter — starts as a single empty table
;; ============================================================

(define current-command-table
  (make-parameter (make-command-table)))

;; ============================================================
;; Operations — all use the current parameter value
;; ============================================================

(define (define-command! name proc)
  (hash-set! (command-table-map (current-command-table)) name proc))

(define (lookup-command name)
  (hash-ref (command-table-map (current-command-table)) name #f))

(define (command-names)
  (sort (hash-keys (command-table-map (current-command-table))) string<?))
