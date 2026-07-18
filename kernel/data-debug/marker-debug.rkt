#lang racket

;; kernel/data-debug/marker-debug.rkt — Marker list debug S-expressions

(require "../data/marker.rkt")

(provide marker-debug-summary)  ;; → "(markers (P0 >) (P1 =) ...)"

(define (marker-debug-summary markers)
  (define parts
    (for/list ([m (in-list markers)])
      (format "(~a ~a)" (marker-pos m) (if (marker-insertion-type m) ">" "="))))
  (format "(markers ~a)" (string-join parts " ")))
