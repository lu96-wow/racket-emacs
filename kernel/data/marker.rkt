#lang racket

;; kernel/data/marker.rkt — Position marker (pure data struct)
;;
;; ============================================================================
;; A marker is a byte position that auto-adjusts when text is inserted
;; or deleted.  Adjustment logic lives in text.rkt — marker.rkt is just
;; the data type.
;; ============================================================================
;;
;; Fields:
;;   pos            : exact-nonnegative-integer? — current byte offset
;;   insertion-type : boolean?
;;     #t — marker stays after newly inserted text (used for point)
;;     #f — marker stays before newly inserted text (used for scroll anchors)
;;
;; ============================================================================

(provide
 make-marker marker?
 marker-pos marker-insertion-type
 set-marker-pos! set-marker-insertion-type!)

;; ============================================================
;; Struct
;; ============================================================

(struct marker
  ([pos #:mutable]              ; exact-nonnegative-integer? — byte offset
   [insertion-type #:mutable])  ; boolean?
  #:transparent)

(define (make-marker [pos 0] [insertion-type #f])
  ;; Contract: pos must be a non-negative integer.
  (unless (and (exact-nonnegative-integer? pos))
    (raise-argument-error 'make-marker "exact-nonnegative-integer?" pos))
  (marker pos insertion-type))
