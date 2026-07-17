#lang racket

;; display/row-cache.rkt — Per-window row cache for incremental redisplay
;;
;; Each cached-row records which buffer byte-range produced which display
;; glyphs (with face-ids already resolved).  When the buffer range hasn't
;; changed, the renderer skips face resolution and directly blits from
;; the cache — pure data composition.
;;
;; Dependencies: display/vbuffer (for blit target), zero kernel deps.

(require "vbuffer.rkt"
         "char-width.rkt")

(provide
 ;; glyph — pre-resolved display element
 glyph? glyph glyph-ch glyph-width glyph-face-id

 ;; cached-row — buffer range → display glyphs
 cached-row? cached-row-buf-start cached-row-buf-end
 cached-row-glyphs cached-row-continued? cached-row-truncated?

 ;; row-cache — per-leaf vector
 row-cache? make-row-cache
 row-cache-rows row-cache-nrows

 ;; queries (pure)
 row-cache-compare      ; cache × idx × buf-start × buf-end → 'exact|'shifted|'stale
 row-cache-valid-row?   ; cache × idx → boolean?

 ;; mutations (in-place for performance)
 row-cache-update!
 row-cache-invalidate!
 row-cache-clear-from!

 ;; blit (cache → vbuffer, pure output)
 row-cache-blit-row!)

;; ============================================================
;; Glyph — one pre-resolved display cell
;; ============================================================

(struct glyph (ch width face-id) #:transparent)
;; ch     — char? what to display
;; width  — exact-positive-integer? columns occupied (1 or 2)
;; face-id — exact-nonnegative-integer? resolved face index

;; ============================================================
;; Cached row — buffer range → glyph vector
;; ============================================================

(struct cached-row
  (buf-start     ; byte-pos — first byte of this display row
   buf-end       ; byte-pos — first byte after this row (exclusive)
   glyphs        ; (vectorof glyph?)
   continued?    ; boolean? — wrapped continuation?
   truncated?)   ; boolean? — was a '$' appended?
  #:transparent)

;; ============================================================
;; Row cache — mutable vector of cached-row
;; ============================================================

(struct row-cache
  ([rows #:mutable]  ; (vectorof (or/c cached-row? #f))
   [nrows #:mutable]) ; number of valid (non-#f) rows
  #:transparent)

(define (make-row-cache max-rows)
  (row-cache (make-vector max-rows #f) 0))

;; ============================================================
;; row-cache-compare — is cached row still valid?
;; ============================================================

(define (row-cache-compare cache row-idx buf-start buf-end)
  ;; Compare cache[row] against current buffer range.
  ;; Returns:
  ;;   'exact   — buffer range identical, can reuse
  ;;   'shifted — same start, different end (more text added/deleted)
  ;;   'stale   — completely different, must rebuild
  (define rows (row-cache-rows cache))
  (cond [(>= row-idx (vector-length rows)) 'stale]
        [else
         (define cr (vector-ref rows row-idx))
         (cond [(not cr) 'stale]
               [(= (cached-row-buf-start cr) buf-start)
                (if (= (cached-row-buf-end cr) buf-end)
                    'exact
                    'shifted)]
               [else 'stale])]))

;; ============================================================
;; row-cache-valid-row?
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
    (error 'row-cache-update! "row index out of bounds: ~a >= ~a"
           row-idx (vector-length rows)))
  (vector-set! rows row-idx
    (cached-row buf-start buf-end glyphs continued? truncated?))
  (set-row-cache-nrows! cache (max (row-cache-nrows cache) (add1 row-idx))))

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
;; row-cache-blit-row! — write cached glyphs into vbuffer
;; ============================================================

(define (row-cache-blit-row! vb row cache row-idx)
  ;; Pure output: reads cache, writes vb cells.
  ;; Handles wide-char skip.  Appends '$' if truncated.
  (define rows (row-cache-rows cache))
  (define cr (and (< row-idx (vector-length rows))
                  (vector-ref rows row-idx)))
  (when cr
    (define glyphs (cached-row-glyphs cr))
    (define max-col (vbuffer-cols vb))
    (let loop ([c 0] [g 0] [skip? #f])
      (when (and (< c max-col) (< g (vector-length glyphs)))
        (define gv (vector-ref glyphs g))
        (if skip?
            (loop (add1 c) (add1 g) #f)
            (begin
              (vbuffer-put-char! vb row c (glyph-ch gv)
                                 #:face-id (glyph-face-id gv))
              (loop (+ c (glyph-width gv))
                    (add1 g)
                    (= (glyph-width gv) 2))))))
    (when (cached-row-truncated? cr)
      (vbuffer-put-char! vb row (sub1 max-col) #\$)))
  vb)

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)

  (test-case "make and compare"
    (let ([c (make-row-cache 5)])
      (check-equal? (row-cache-compare c 0 0 10) 'stale)
      (row-cache-update! c 0 0 10 (vector (glyph #\a 1 0) (glyph #\b 1 0)))
      (check-equal? (row-cache-compare c 0 0 10) 'exact)
      (check-equal? (row-cache-compare c 0 0 12) 'shifted)
      (check-equal? (row-cache-compare c 0 5 10) 'stale)))

  (test-case "invalidate and clear"
    (let ([c (make-row-cache 5)])
      (row-cache-update! c 0 0 5 (vector (glyph #\x 1 0)))
      (row-cache-update! c 1 5 10 (vector (glyph #\y 1 0)))
      (check-true (row-cache-valid-row? c 0))
      (check-true (row-cache-valid-row? c 1))
      (row-cache-clear-from! c 1)
      (check-true (row-cache-valid-row? c 0))
      (check-false (row-cache-valid-row? c 1))
      (row-cache-invalidate! c)
      (check-false (row-cache-valid-row? c 0))))

  (test-case "blit into vbuffer"
    (let ([vb (make-vbuffer 1 5)]
          [c  (make-row-cache 2)])
      (row-cache-update! c 0 0 3
        (vector (glyph #\H 1 7) (glyph #\i 1 7) (glyph #\! 1 7)))
      (row-cache-blit-row! vb 0 c 0)
      (check-equal? (vbuffer-row->string vb 0) "Hi!  ")
      (check-equal? (cell-face-id (vector-ref (vbuffer-cells vb) 0)) 7)
      (check-equal? (cell-face-id (vector-ref (vbuffer-cells vb) 1)) 7)))

  (test-case "blit wide char"
    (let ([vb (make-vbuffer 1 5)]
          [c  (make-row-cache 2)])
      (row-cache-update! c 0 0 4
        (vector (glyph #\a 1 0) (glyph #\中 2 0) (glyph #\b 1 0)))
      (row-cache-blit-row! vb 0 c 0)
      (check-equal? (cell-ch (vector-ref (vbuffer-cells vb) 0)) #\a)
      (check-equal? (cell-ch (vector-ref (vbuffer-cells vb) 1)) #\中)
      ;; cell at col 2 should be space (skipped by wide char)
      (check-equal? (cell-ch (vector-ref (vbuffer-cells vb) 3)) #\b)))

  (test-case "blit with truncation marker"
    (let ([vb (make-vbuffer 1 5)]
          [c  (make-row-cache 2)])
      (row-cache-update! c 0 0 2
        (vector (glyph #\a 1 0) (glyph #\b 1 0)) #f #t)
      (row-cache-blit-row! vb 0 c 0)
      (check-equal? (cell-ch (vector-ref (vbuffer-cells vb) 4)) #\$))))
