#lang racket

;; base/marker.rkt — Position markers that auto-adjust on buffer edits
;;
;; All positions are byte-offsets. marker-buffer = #f means dead.
;; Dependency-free.

(provide
 make-marker marker?
 marker-pos marker-buffer marker-insertion-type
 set-marker-pos! set-marker-buffer!
 adjust-markers-insert! adjust-markers-delete!)

(struct marker
  ([buffer #:mutable]          ; (or/c buffer? #f) — #f = dead
   [pos #:mutable]             ; exact-nonnegative-integer? — byte offset
   [insertion-type #:mutable]) ; boolean? — stay after inserted text?
  #:transparent)

(define (make-marker [pos 0] [insertion-type #f] [buf #f])
  (marker buf pos insertion-type))

(define (adjust-markers-insert! markers byte-pos byte-len)
  ;; Called after inserting byte-len bytes at byte-pos.
  (for ([m (in-list markers)])
    (define p (marker-pos m))
    (cond [(< p byte-pos) (void)]
          [(= p byte-pos)
           (when (marker-insertion-type m)
             (set-marker-pos! m (+ p byte-len)))]
          [else (set-marker-pos! m (+ p byte-len))])))

(define (adjust-markers-delete! markers from to)
  ;; Called after deleting bytes [from, to).
  (define byte-len (- to from))
  (for ([m (in-list markers)])
    (define p (marker-pos m))
    (cond [(< p from) (void)]
          [(< p to)   (set-marker-pos! m from)]
          [else       (set-marker-pos! m (- p byte-len))])))
