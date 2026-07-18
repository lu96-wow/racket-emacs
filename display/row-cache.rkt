#lang racket

;; display/row-cache.rkt — Per-leaf row cache for incremental redisplay
;;
;; ============================================================================
;; Each cached entry is a vbuffer-row: cells + buffer byte-range + flags.
;; When the buffer range hasn't changed, the renderer skips face resolution
;; and directly reuses the cached row — pure data composition.
;;
;; ============================================================================
;; Comparison
;; ============================================================================
;;
;;   'exact   — same buf-start AND buf-end → cache hit, reuse row
;;   'shifted — same buf-start, different buf-end → must re-render
;;   'stale   — different buf-start → must re-render
;;
;; ============================================================================
;; Dependencies: display/vbuffer (vbuffer-row struct)
;; ============================================================================

(require "vbuffer.rkt")

(provide
 ;; ── row-cache ──
 row-cache? make-row-cache
 row-cache-rows row-cache-nrows

 ;; ── queries (pure) ──
 row-cache-compare      ;; cache × idx × buf-start × buf-end → 'exact|'shifted|'stale
 row-cache-cached-row   ;; cache × idx → vbuffer-row? | #f

 ;; ── mutations ──
 row-cache-store!       ;; cache × idx × vbuffer-row? → void
 row-cache-invalidate!  ;; cache → void
 row-cache-clear-from!  ;; cache × idx → void

 ;; ── blit (cached row → vbuffer) ──
 row-cache-blit-row!)    ;; vb × row × cache × idx → void

;; ============================================================
;; Row cache — mutable vector of vbuffer-row?
;; ============================================================

(struct row-cache
  ([rows #:mutable]   ; (vectorof (or/c vbuffer-row? #f))
   [nrows #:mutable]) ; number of valid (non-#f) rows
  #:transparent)

(define (make-row-cache max-rows)
  (unless (exact-positive-integer? max-rows)
    (raise-argument-error 'make-row-cache "positive integer" max-rows))
  (row-cache (make-vector max-rows #f) 0))

;; ============================================================
;; row-cache-compare
;; ============================================================

(define (row-cache-compare cache row-idx buf-start buf-end)
  (define rows (row-cache-rows cache))
  (cond [(>= row-idx (vector-length rows)) 'stale]
        [else
         (define cr (vector-ref rows row-idx))
         (cond [(not cr) 'stale]
               [(= (vbuffer-row-buf-start cr) buf-start)
                (if (= (vbuffer-row-buf-end cr) buf-end)
                    'exact
                    'shifted)]
               [else 'stale])]))

(define (row-cache-cached-row cache row-idx)
  (define rows (row-cache-rows cache))
  (and (< row-idx (vector-length rows))
       (vector-ref rows row-idx)))

;; ============================================================
;; row-cache-store!
;; ============================================================

(define (row-cache-store! cache row-idx vrow)
  (unless (vbuffer-row? vrow)
    (raise-argument-error 'row-cache-store! "vbuffer-row?" vrow))
  (define rows (row-cache-rows cache))
  (when (>= row-idx (vector-length rows))
    (error 'row-cache-store! "row index out of bounds: ~a >= ~a"
           row-idx (vector-length rows)))
  (vector-set! rows row-idx vrow)
  (set-row-cache-nrows! cache (max (row-cache-nrows cache) (add1 row-idx))))

;; ============================================================
;; row-cache-invalidate! / row-cache-clear-from!
;; ============================================================

(define (row-cache-invalidate! cache)
  (define rows (row-cache-rows cache))
  (for ([i (in-range (vector-length rows))])
    (vector-set! rows i #f))
  (set-row-cache-nrows! cache 0))

(define (row-cache-clear-from! cache row-idx)
  (define rows (row-cache-rows cache))
  (for ([i (in-range row-idx (vector-length rows))])
    (vector-set! rows i #f))
  (set-row-cache-nrows! cache (min (row-cache-nrows cache) row-idx)))

;; ============================================================
;; row-cache-blit-row! — write cached vbuffer-row into vbuffer
;; ============================================================

(define (row-cache-blit-row! vb row cache row-idx)
  (define cr (row-cache-cached-row cache row-idx))
  (when cr
    (vector-set! (vbuffer-rows vb) row cr))
  vb)
