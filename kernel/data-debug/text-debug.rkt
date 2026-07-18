#lang racket

;; kernel/data-debug/text-debug.rkt — Text debug S-expression

(require "../data/text.rkt"
         "gap-debug.rkt"
         "marker-debug.rkt")

(provide text-debug-summary)  ;; → "(text (gap ...) (markers ...))"

(define (text-debug-summary tx)
  (format "(text ~a ~a)"
          (gap-debug-summary (text-gap tx))
          (marker-debug-summary (text-markers tx))))
