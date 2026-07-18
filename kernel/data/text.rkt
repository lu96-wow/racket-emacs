#lang racket

;; kernel/data/text.rkt — Atomic text: gap buffer + position markers
;;
;; ============================================================================
;; A `text` is the fundamental mutable unit.  It owns a gap buffer and a
;; set of markers.  All mutations go through two entry points that update
;; both text and markers.
;;
;; IMPORTANT: text.rkt knows NOTHING about faces.  The gap buffer internally
;; maintains a faces array (colocated with bytes for O(1) access), but
;; text-insert! and text-delete! only deal with bytes.  Face management is
;; a higher-layer concern (buffer.rkt, colorer, bracket-cache).
;;
;; ============================================================================

(require "gap.rkt"
         "marker.rkt")

(provide
 text? text-gap text-markers make-text
 text-length text-byte-ref text-subbytes
 text-insert! text-delete!
 adjust-markers-insert! adjust-markers-delete!
 text-marker! text-marker-kill! text-marker-pos text-set-marker-pos!)

;; ============================================================
;; Struct
;; ============================================================

(struct text
  ([gap #:mutable]      ; gap-buffer? — bytes + face-ids (faces are internal)
   [markers #:mutable]) ; (listof marker?) — tracked positions
  #:transparent)

(define (make-text [initial ""])
  (unless (string? initial)
    (raise-argument-error 'make-text "string?" initial))
  (text (make-gap-buffer initial) '()))

;; ============================================================
;; Pure Queries — delegate to gap
;; ============================================================

(define (text-length tx)         (gap-length (text-gap tx)))
(define (text-byte-ref tx pos)   (gap-byte-ref (text-gap tx) pos))
(define (text-subbytes tx f t)   (gap-subbytes (text-gap tx) f t))

;; ============================================================
;; Public Mutation: text-insert! — bytes only, no faces
;; ============================================================

(define (text-insert! tx byte-pos bs)
  (unless (bytes? bs)
    (raise-argument-error 'text-insert! "bytes?" bs))
  (define blen (bytes-length bs))
  (define real-pos (max 0 (min byte-pos (text-length tx))))
  (when (positive? blen)
    (gap-insert! (text-gap tx) real-pos bs)
    (adjust-markers-insert! (text-markers tx) real-pos blen)))

;; ============================================================
;; Public Mutation: text-delete! — bytes only, returns void
;; ============================================================

(define (text-delete! tx from to)
  (define max-to (text-length tx))
  (define real-from (max 0 from))
  (define real-to (min to max-to))
  (define count (- real-to real-from))
  (when (positive? count)
    (gap-delete! (text-gap tx) real-from real-to)
    (adjust-markers-delete! (text-markers tx) real-from real-to)))

;; ============================================================
;; Marker Adjustment
;; ============================================================

(define (adjust-markers-insert! markers byte-pos byte-len)
  (for ([m (in-list markers)])
    (define p (marker-pos m))
    (cond [(< p byte-pos) (void)]
          [(= p byte-pos)
           (when (marker-insertion-type m)
             (set-marker-pos! m (+ p byte-len)))]
          [else (set-marker-pos! m (+ p byte-len))])))

(define (adjust-markers-delete! markers from to)
  (define byte-len (- to from))
  (for ([m (in-list markers)])
    (define p (marker-pos m))
    (cond [(< p from) (void)]
          [(< p to)   (set-marker-pos! m from)]
          [else       (set-marker-pos! m (- p byte-len))])))

;; ============================================================
;; Marker Management
;; ============================================================

(define (text-marker! tx pos [insertion-type #f])
  (unless (and (exact-nonnegative-integer? pos) (<= pos (text-length tx)))
    (raise-argument-error 'text-marker!
                          (format "valid position in [0, ~a]" (text-length tx)) pos))
  (define m (make-marker pos insertion-type))
  (set-text-markers! tx (cons m (text-markers tx)))
  m)

(define (text-marker-kill! tx m)
  (define old (text-markers tx))
  (define new (remove m old))
  (set-text-markers! tx new)
  (not (= (length new) (length old))))

(define (text-marker-pos tx m)
  (marker-pos m))

(define (text-set-marker-pos! tx m pos)
  (unless (and (exact-nonnegative-integer? pos) (<= pos (text-length tx)))
    (raise-argument-error 'text-set-marker-pos!
                          (format "valid position in [0, ~a]" (text-length tx)) pos))
  (set-marker-pos! m pos))
