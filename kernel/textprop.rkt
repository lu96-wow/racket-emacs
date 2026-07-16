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

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)

  (let ([tp (make-text-properties)])
    (textprop-put! tp 0 10 'face 'keyword)
    (check-equal? (textprop-get tp 5 'face) 'keyword)
    (check-equal? (textprop-face-at tp 5) 'keyword)
    (check-equal? (textprop-get tp 15 'face 'default) 'default))

  (let ([tp (make-text-properties)])
    (textprop-put! tp 5 10 'face 'comment)
    (textprop-adjust-insert! tp 5 3)
    (check-equal? (textprop-face-at tp 3) #f)
    (check-equal? (textprop-face-at tp 10) 'comment)
    (check-equal? (textprop-face-at tp 12) 'comment))

  (let ([tp (make-text-properties)])
    (textprop-put! tp 10 20 'face 'string)
    (textprop-adjust-delete! tp 5 8)
    (check-equal? (textprop-face-at tp 7) 'string)
    (check-equal? (textprop-face-at tp 15) 'string))

  (let ([tp (make-text-properties)])
    (textprop-put! tp 0 20 'face 'comment)
    (textprop-put! tp 0 20 'syntax-table 'string)
    (check-equal? (textprop-get tp 5 'face) 'comment)
    (check-equal? (textprop-get tp 5 'syntax-table) 'string)))
