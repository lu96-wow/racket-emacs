#lang racket

;; kernel/text.rkt — Atomic text: gap buffer + position markers
;;
;; A `text` is the fundamental mutable unit of the editor.
;; It owns a gap buffer and a set of markers.  All mutations
;; go through a single entry point that updates both.
;;
;; Higher layers (protocol/buffer.rkt) add undo, hooks, and
;; change-tracking on top of this atom.
;;
;; Exports are deliberately split into:
;;   - query   (pure, no mutation)
;;   - mutation (single entry point)
;;   - marker  (create, query, kill)
;;
;; adjust-markers-insert! and adjust-markers-delete! are also
;; exported so they can be tested independently.

(require "gap.rkt"
         "marker.rkt")

(provide
 ;; struct + constructor
 text? text-gap text-markers
 make-text
 ;; queries
 text-length text-byte-ref text-subbytes
 ;; mutations
 text-insert! text-delete!
 ;; marker adjustment
 adjust-markers-insert! adjust-markers-delete!
 ;; marker management
 text-marker! text-marker-kill! text-marker-pos text-set-marker-pos!)

;; ============================================================
;; Struct
;; ============================================================

(struct text
  ([gap #:mutable]      ; gap-buffer? — the bytes
   [markers #:mutable]) ; (listof marker?) — tracked positions
  #:transparent)

(define (make-text [initial ""])
  (text (make-gap-buffer initial) '()))

;; ============================================================
;; Queries — delegate to gap
;; ============================================================

(define (text-length tx)         (gap-length (text-gap tx)))
(define (text-byte-ref tx pos)   (gap-byte-ref (text-gap tx) pos))
(define (text-subbytes tx f t)   (gap-subbytes (text-gap tx) f t))

;; ============================================================
;; Mutations — single entry point
;; ============================================================

(define (text-insert! tx byte-pos bs)
  (define blen (bytes-length bs))
  (define real-pos (max 0 (min byte-pos (text-length tx))))
  (when (positive? blen)
    (gap-insert! (text-gap tx) real-pos bs)
    (adjust-markers-insert! (text-markers tx) real-pos blen)))

(define (text-delete! tx from to)
  (define max-to (text-length tx))
  (define real-from (max 0 from))
  (define real-to (min to max-to))
  (define count (- real-to real-from))
  (when (positive? count)
    (gap-delete! (text-gap tx) real-from real-to)
    (adjust-markers-delete! (text-markers tx) real-from real-to)))

;; ============================================================
;; Marker adjustment — pure functions, independently testable
;; ============================================================

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

;; ============================================================
;; Marker management
;; ============================================================

(define (text-marker! tx pos [insertion-type #f])
  (define m (make-marker pos insertion-type))
  (set-text-markers! tx (cons m (text-markers tx)))
  m)

(define (text-marker-kill! tx m)
  (set-text-markers! tx (remove m (text-markers tx))))

(define (text-marker-pos tx m)
  (marker-pos m))

(define (text-set-marker-pos! tx m pos)
  (set-marker-pos! m pos))

