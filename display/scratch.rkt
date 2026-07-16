#lang racket

;; display/scratch.rkt — Per-window row cache for incremental redisplay
;;
;; Each cached-row remembers which buffer range produced which visual content.
;; The renderer can skip rebuilding rows whose buffer range hasn't changed.
;; This is the key optimization that Emacs's "try_window_reusing_current_matrix" enables.

(provide
 ;; row cache
 row-cache? make-row-cache
 cached-row? cached-row-buf-start cached-row-buf-end
 cached-row-continued? cached-row-truncated? cached-row-glyphs

 ;; operations
 row-cache-compare    ; compare cache vs current iterator position
 row-cache-valid-row? ; can we reuse this row?
 row-cache-update!    ; update a row in the cache
 row-cache-invalidate! ; clear the cache
 row-cache-clear-from! ; clear from a specific row onward

 ;; glyph
 glyph? glyph glyph-ch glyph-width glyph-face-id)

;; ============================================================
;; Glyph — a display element
;; ============================================================

(struct glyph (ch width face-id) #:transparent)

;; ============================================================
;; Cached row
;; ============================================================

(struct cached-row
  (buf-start     ; integer — byte position where this row starts in buffer
   buf-end       ; integer — byte position where this row ends (exclusive)
   glyphs        ; (vectorof glyph?) — what to display
   continued?    ; boolean? — is this a wrapped continuation?
   truncated?)   ; boolean? — was it truncated with '$'?
  #:transparent)

;; ============================================================
;; Row cache — per-window vector of cached-row
;; ============================================================

(struct row-cache
  ([rows #:mutable]  ; (vectorof (or/c cached-row? #f))
   [nrows #:mutable]) ; how many valid rows
  #:transparent)

(define (make-row-cache max-rows)
  (row-cache (make-vector max-rows #f) 0))

;; ============================================================
;; row-cache-compare — check if row-cache[i] matches current iter
;; ============================================================
;; Returns:
;;   'exact   — buffer range matches exactly, can reuse
;;   'shifted — buffer start matches but end differs (more text added)
;;   'stale   — completely different, must rebuild

(define (row-cache-compare cache row-idx buf-start buf-end)
  (define rows (row-cache-rows cache))
  (when (>= row-idx (vector-length rows))
    'stale)
  (define cr (vector-ref rows row-idx))
  (cond
    [(not cr) 'stale]
    [(= (cached-row-buf-start cr) buf-start)
     (if (= (cached-row-buf-end cr) buf-end)
         'exact
         'shifted)]
    [else 'stale]))

;; ============================================================
;; row-cache-valid-row? — can we completely skip rebuilding?
;; ============================================================

(define (row-cache-valid-row? cache row-idx)
  (define rows (row-cache-rows cache))
  (and (< row-idx (vector-length rows))
       (vector-ref rows row-idx)
       #t))

;; ============================================================
;; row-cache-update!
;; ============================================================

(define (row-cache-update! cache row-idx buf-start buf-end glyphs
                          [continued? #f] [truncated? #f])
  (define rows (row-cache-rows cache))
  (when (>= row-idx (vector-length rows))
    (error 'row-cache-update! "row index out of bounds"))
  (vector-set! rows row-idx
    (cached-row buf-start buf-end glyphs continued? truncated?)))

;; ============================================================
;; row-cache-invalidate!
;; ============================================================

(define (row-cache-invalidate! cache)
  (define rows (row-cache-rows cache))
  (for ([i (in-range (vector-length rows))])
    (vector-set! rows i #f))
  (set-row-cache-nrows! cache 0))

;; ============================================================
;; row-cache-clear-from!
;; ============================================================

(define (row-cache-clear-from! cache row-idx)
  (define rows (row-cache-rows cache))
  (for ([i (in-range row-idx (vector-length rows))])
    (vector-set! rows i #f))
  (set-row-cache-nrows! cache (min (row-cache-nrows cache) row-idx)))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)

  (let ([cache (make-row-cache 5)])
    (check-equal? (row-cache-compare cache 0 0 10) 'stale)
    (row-cache-update! cache 0 0 10 (vector (glyph #\a 1 0) (glyph #\b 1 0)))
    (check-equal? (row-cache-compare cache 0 0 10) 'exact)
    (check-equal? (row-cache-compare cache 0 0 12) 'shifted)
    (check-equal? (row-cache-compare cache 0 5 10) 'stale)))
