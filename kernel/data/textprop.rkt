#lang racket

;; kernel/data/textprop.rkt — Text Properties (interval-map based)
;;
;; ============================================================================
;; Stores key→value metadata on byte ranges.  Each interval holds a
;; (hasheq key value) so multiple properties can coexist on the same range.
;;
;; The underlying interval-map from data/interval-map auto-adjusts when
;; expand!/contract! is called.  Adjustment calls are EXPLICIT — the
;; caller (buffer.rkt) must call textprop-adjust-insert!/delete! after
;; every text mutation.  No hooks, no implicit behaviour.
;;
;; ============================================================================
;; Computation vs Application
;; ============================================================================
;;
;;   ── Pure Queries ──
;;     textprop-get       pos key [default] → value
;;     textprop-face-at   pos → face-name or #f
;;
;;   ── Mutations (write to interval-map) ──
;;     textprop-put!          from to key value    — set property on range
;;     textprop-remove!       from to              — clear all properties on range
;;     textprop-remove-key!   from to key          — clear one key on range
;;
;;   ── Adjustment (explicit, called after text mutations) ──
;;     textprop-adjust-insert!   byte-pos byte-len
;;     textprop-adjust-delete!    from to
;;
;; ============================================================================
;; Contract
;; ============================================================================
;;
;;   All from/to positions are validated as exact integers.
;;   textprop-remove-key! uses inexact→exact (round ...) as a safety net
;;   against floating-point positions that may leak from the colorer.
;;
;; ============================================================================

(require data/interval-map)

(provide
 ;; ── struct ──
 text-properties? make-text-properties
 text-properties-map

 ;; ── mutations ──
 textprop-put!
 textprop-remove!
 textprop-remove-key!

 ;; ── queries ──
 textprop-get
 textprop-face-at

 ;; ── adjustment ──
 textprop-adjust-insert!
 textprop-adjust-delete!)

;; ============================================================
;; Struct
;; ============================================================

(struct text-properties
  ([map #:mutable])  ; interval-map? — byte-range → (hasheq key value)
  #:transparent)

(define (make-text-properties)
  (text-properties (make-interval-map)))

;; ============================================================
;; Internal: validate a byte range
;; ============================================================

(define (validate-byte-range! from to context)
  ;; Ensure from and to are exact integers with from ≤ to.
  ;; If values are floating (from colorer arithmetic), snap them.
  (define f (if (integer? from) from (inexact->exact (round from))))
  (define t (if (integer? to)   to   (inexact->exact (round to))))
  (unless (and (exact-integer? f) (exact-integer? t) (>= t f))
    (raise-argument-error context
                          "valid [from, to] byte range with from ≤ to"
                          (list from to)))
  (values f t))

;; ============================================================
;; Write — store (hasheq key value) on [from, to)
;; ============================================================

(define (textprop-put! tp from to key value)
  ;; Set KEY → VALUE on the byte range [from, to).
  ;; Merges with existing properties at overlapping ranges.
  (define-values (f t) (validate-byte-range! from to 'textprop-put!))
  (when (< f t)
    (define im (text-properties-map tp))
    (define existing
      (interval-map-ref im f (λ () (hasheq))))
    (define merged (hash-set existing key value))
    (interval-map-set! im f t merged)))

;; ============================================================
;; Read — pure queries
;; ============================================================

(define (textprop-get tp pos key [default #f])
  ;; Get property KEY at byte position `pos`.
  (define p (if (integer? pos) pos (inexact->exact (round pos))))
  (define h (interval-map-ref (text-properties-map tp) p (λ () (hasheq))))
  (hash-ref h key default))

(define (textprop-face-at tp pos)
  ;; Get the 'face property at byte position `pos`.
  ;; Returns face-name or #f.
  (textprop-get tp pos 'face #f))

;; ============================================================
;; Remove — clear properties on a range
;; ============================================================

(define (textprop-remove! tp from to)
  ;; Remove ALL properties on [from, to).
  (define-values (f t) (validate-byte-range! from to 'textprop-remove!))
  (when (< f t)
    (interval-map-remove! (text-properties-map tp) f t)))

(define (textprop-remove-key! tp from to key)
  ;; Remove only property KEY on [from, to).
  ;; Other properties on the same ranges are preserved.
  ;; Walks affected intervals and splits/re-merges as needed.
  (define-values (f t) (validate-byte-range! from to 'textprop-remove-key!))
  (when (< f t)
    (define im (text-properties-map tp))
    (let loop ([pos f])
      (when (< pos t)
        (define-values (ivl-start ivl-end h)
          (interval-map-ref/bounds im pos (λ () (hasheq))))

        (if (not ivl-start)
            ;; No interval covers `pos` → advance
            (loop (add1 pos))

            (let* ([hit-start (max ivl-start f)]
                   [hit-end   (min ivl-end t)])
              (when (and (< hit-start hit-end)
                         (hash-has-key? h key))
                ;; This interval has the key → remove it
                (define h2 (hash-remove h key))

                ;; Remove old interval, re-insert splits
                (interval-map-remove! im ivl-start ivl-end)

                ;; Left segment (before hit range)
                (when (< ivl-start hit-start)
                  (interval-map-set! im ivl-start hit-start h))

                ;; Middle segment (hit range, with key removed)
                (when (and (< hit-start hit-end)
                           (not (hash-empty? h2)))
                  (interval-map-set! im hit-start hit-end h2))

                ;; Right segment (after hit range)
                (when (< hit-end ivl-end)
                  (interval-map-set! im hit-end ivl-end h)))

              ;; Advance past the processed interval
              (loop (max (add1 pos) hit-end))))))))

;; ============================================================
;; Adjustment — explicit, called after text mutations
;; ============================================================

(define (textprop-adjust-insert! tp byte-pos byte-len)
  ;; Called after inserting byte-len bytes at byte-pos.
  ;; Expands the interval-map to create a gap for the new text.
  (when (and (exact-integer? byte-pos) (exact-integer? byte-len)
             (positive? byte-len))
    (interval-map-expand! (text-properties-map tp)
                          byte-pos (+ byte-pos byte-len))))

(define (textprop-adjust-delete! tp from to)
  ;; Called after deleting bytes [from, to).
  ;; Contracts the interval-map to remove the deleted range.
  (define-values (f t) (validate-byte-range! from to 'textprop-adjust-delete!))
  (when (< f t)
    (interval-map-contract! (text-properties-map tp) f t)))
