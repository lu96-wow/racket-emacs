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
