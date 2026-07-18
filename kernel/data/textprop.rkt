#lang racket

;; kernel/textprop.rkt — Text Properties (interval-map based)
;;
;; Stores key-value metadata on byte ranges.  The underlying
;; interval-map auto-adjusts on expand!/contract! calls.
;; Values stored are (hasheq key value) so multiple properties
;; can coexist on the same range.
;;
;; Adjustment calls are explicit — the caller (buffer.rkt) is
;; responsible for calling textprop-adjust-insert!/delete! after
;; every text mutation.  No hooks, no implicit behaviour.
;;
;; Dependencies: data/interval-map (Racket standard library).

(require data/interval-map)

(provide
 text-properties? make-text-properties
 text-properties-map
 textprop-put!
 textprop-get
 textprop-face-at
 textprop-remove!
 textprop-remove-key!
 textprop-adjust-insert!
 textprop-adjust-delete!)

;; ============================================================
;; Struct
;; ============================================================

(struct text-properties
  ([map #:mutable])
  #:transparent)

(define (make-text-properties)
  (text-properties (make-interval-map)))

;; ============================================================
;; Write — store (hasheq key value) on [from, to)
;; ============================================================

(define (textprop-put! tp from to key value)
  (when (< from to)
    (define existing
      (interval-map-ref (text-properties-map tp) from (λ () (hasheq))))
    (define merged (hash-set existing key value))
    (interval-map-set! (text-properties-map tp) from to merged)))

;; ============================================================
;; Read
;; ============================================================

(define (textprop-get tp pos key [default #f])
  (define h (interval-map-ref (text-properties-map tp) pos (λ () (hasheq))))
  (hash-ref h key default))

(define (textprop-face-at tp pos)
  (define h (interval-map-ref (text-properties-map tp) pos (λ () (hasheq))))
  (hash-ref h 'face #f))

;; ============================================================
;; Remove
;; ============================================================

(define (textprop-remove! tp from to)
  (when (< from to)
    (interval-map-remove! (text-properties-map tp) from to)))

(define (textprop-remove-key! tp from to key)
  ;; Remove only `key` from properties in [from, to).
  ;; Walks only intervals that overlap [from, to) using
  ;; interval-map-ref/bounds — O(k) where k = affected intervals.
  (when (< from to)
    (define im (text-properties-map tp))
    (let loop ([pos from])
      (when (< pos to)
        ;; interval-map-ref/bounds returns: (values start end value)
        (define-values (ivl-start ivl-end h)
          (interval-map-ref/bounds im pos (λ () (hasheq))))
        (define hit-start (max (or ivl-start 0) from))
        (define hit-end   (min (or ivl-end to) to))
        (when (and (< hit-start hit-end) (hash-has-key? h key))
          (define h2 (hash-remove h key))
          (interval-map-remove! im ivl-start ivl-end)
          (unless (hash-empty? h2)
            (interval-map-set! im ivl-start ivl-end h2)))
        ;; Advance to next interval
        (define next (max (add1 pos) hit-end))
        (when (> next pos) (loop next))))))

;; ============================================================
;; Adjustment — explicit, called by buffer.rkt after each mutation
;; ============================================================

(define (textprop-adjust-insert! tp byte-pos byte-len)
  (when (positive? byte-len)
    (interval-map-expand! (text-properties-map tp)
                          byte-pos (+ byte-pos byte-len))))

(define (textprop-adjust-delete! tp from to)
  (when (< from to)
    (interval-map-contract! (text-properties-map tp) from to)))
