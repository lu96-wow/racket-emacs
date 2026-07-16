#lang racket

;; kernel/marker/marker.rkt — Position marker (pure data)
;;
;; A marker is a byte position that auto-adjusts when text is
;; inserted or deleted.  It does NOT reference any buffer/gap.
;; Adjustment logic lives in kernel/text.rkt.

(provide
 make-marker marker?
 marker-pos marker-insertion-type
 set-marker-pos! set-marker-insertion-type!)

(struct marker
  ([pos #:mutable]              ; exact-nonnegative-integer? — byte offset
   [insertion-type #:mutable])  ; boolean? — #t = stay after inserted text
  #:transparent)

(define (make-marker [pos 0] [insertion-type #f])
  (marker pos insertion-type))
