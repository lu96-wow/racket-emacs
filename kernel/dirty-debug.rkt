#lang racket

;; kernel/dirty-debug.rkt — Dirty buffer change marker debug S-expression

(require "dirty.rkt")

(provide dirty-debug-summary)  ;; → "(dirty clean)" | "(dirty (edit POS DELTA))"

(define (dirty-debug-summary db)
  (define chg (dirty-change db))
  (if chg
      (format "(dirty (edit ~a ~a))" (car chg) (cdr chg))
      "(dirty clean)"))
