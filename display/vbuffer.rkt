#lang racket

;; display/vbuffer.rkt — Virtual screen buffer with buffer-to-screen mapping
;;
;; ============================================================================
;; A vbuffer is a screen-sized grid of cells that acts as a WINDOW onto
;; a gap-buffer.  Each row knows which buffer byte range it displays,
;; enabling screen↔buffer position queries directly from the vbuffer.
;;
;; ============================================================================
;; Architecture
;; ============================================================================
;;
;;   gap-buffer        — the underlying text + face data
;;   layout (pure)     — computes: which buffer bytes → which screen rows
;;   render (pure)     — fills vbuffer cells + attaches buffer mapping metadata
;;   vbuffer           — the result: screen grid + per-row byte ranges
;;   terminal (output) — diffs vbuffer against cache, emits ANSI
;;
;; The vbuffer is the SINGLE SOURCE OF TRUTH for screen↔buffer mapping
;; after rendering.  Mouse input, cursor positioning, and scroll all
;; read from the vbuffer, not from layout or render intermediates.
;;
;; ============================================================================
;; Dependencies
;; ============================================================================
;;
;;   kernel/data/gap.rkt         — gap-buffer? type
;;   kernel/data/query.rkt       — gap-next-char-pos
;;   kernel/data/char-width.rkt  — char-display-width
;; ============================================================================

(require "../kernel/data/gap.rkt"
         "../kernel/data/query.rkt"
         "../kernel/data/char-width.rkt")

(provide
 ;; ── cell ──
 cell? cell cell-ch cell-attrs cell-face-id

 ;; ── vbuffer-row ──
 vbuffer-row? vbuffer-row vbuffer-row-cells
 vbuffer-row-buf-start vbuffer-row-buf-end
 vbuffer-row-continued? vbuffer-row-truncated?
 vbuffer-row-display-len

 ;; ── vbuffer ──
 vbuffer? vbuffer-rows vbuffer-nrows vbuffer-ncols vbuffer-gap
 make-vbuffer

 ;; ── mutation ──
 vbuffer-fill-row!

 ;; ── pure queries ──
 vbuffer-cell-at
 vbuffer-row-byte-range
 vbuffer-xy->byte-pos
 vbuffer-byte-pos->xy

 ;; ── comparison ──
 vbuffer-row-changed? vbuffer-cell-equal?

 ;; ── blit ──
 vbuffer-blit!

 ;; ── serialization ──
 vbuffer-row->string

 ;; ── iteration ──
 in-vbuffer-rows)

;; ============================================================
;; Cell — one screen position
;; ============================================================

(struct cell
  (ch            ; char? — character to display
   [attrs #:mutable]      ; (or/c #f symbol? (listof symbol?))
   [face-id #:mutable])   ; exact-nonnegative-integer? — face index
  #:transparent)

;; ============================================================
;; vbuffer-row — one screen row with buffer byte-range metadata
;; ============================================================

(struct vbuffer-row
  (cells        ; (vectorof cell?) — ncols cells
   buf-start    ; exact-nonnegative-integer? — first buffer byte on this row
   buf-end      ; exact-nonnegative-integer? — first buffer byte AFTER this row
   continued?   ; boolean? — wrapped continuation of a logical line?
   truncated?   ; boolean? — '$' at end because line doesn't fit?
   display-len) ; exact-nonnegative-integer? — actual columns used
  #:transparent)

;; ============================================================
;; vbuffer — screen grid + buffer reference
;; ============================================================

(struct vbuffer
  (rows    ; (vectorof vbuffer-row?) — screen rows, top-to-bottom
   nrows   ; exact-positive-integer? — number of rows
   ncols   ; exact-positive-integer? — number of columns
   gap     ; (or/c gap-buffer? #f) — the buffer this vbuffer views
   )
  #:transparent)

(define (make-vbuffer nrows ncols [gap #f])
  (unless (and (exact-positive-integer? nrows) (exact-positive-integer? ncols))
    (raise-argument-error 'make-vbuffer "positive integers" (list nrows ncols)))
  (define rows (make-vector nrows #f))
  (vbuffer rows nrows ncols gap))

;; ============================================================
;; vbuffer-fill-row! — write one row of cells + metadata
;; ============================================================

(define (vbuffer-fill-row! vb row
                           cells buf-start buf-end
                           continued? truncated? display-len)
  (unless (and (>= row 0) (< row (vbuffer-nrows vb)))
    (raise-argument-error 'vbuffer-fill-row!
                          (format "row in [0, ~a)" (vbuffer-nrows vb)) row))
  (unless (= (vector-length cells) (vbuffer-ncols vb))
    (raise-argument-error 'vbuffer-fill-row!
                          (format "cells vector of length ~a" (vbuffer-ncols vb))
                          (vector-length cells)))
  (vector-set! (vbuffer-rows vb) row
    (vbuffer-row cells buf-start buf-end continued? truncated? display-len)))

;; ============================================================
;; Pure queries
;; ============================================================

(define (vbuffer-cell-at vb row col)
  (define rows (vbuffer-rows vb))
  (and (>= row 0) (< row (vbuffer-nrows vb))
       (>= col 0) (< col (vbuffer-ncols vb))
       (let ([vr (vector-ref rows row)])
         (and vr (vector-ref (vbuffer-row-cells vr) col)))))

(define (vbuffer-row-byte-range vb row)
  (define rows (vbuffer-rows vb))
  (and (>= row 0) (< row (vbuffer-nrows vb))
       (let ([vr (vector-ref rows row)])
         (and vr (values (vbuffer-row-buf-start vr)
                         (vbuffer-row-buf-end vr))))))

;; vbuffer-row accessors (vbuffer-row-continued? and vbuffer-row-truncated?)
;; are auto-generated by the struct — no wrapper needed.

;; ============================================================
;; Screen → Buffer: vbuffer-xy->byte-pos
;; ============================================================
;;
;; Walk the vbuffer row from column 0 to the target column,
;; tracking the buffer byte position.  Wide characters (width=2)
;; occupy the column where the char is stored; the next column
;; is a skip marker (space with fid=0).

(define (vbuffer-xy->byte-pos vb row col)
  (define rows (vbuffer-rows vb))
  (cond
    [(or (< row 0) (>= row (vbuffer-nrows vb))
         (< col 0) (>= col (vbuffer-ncols vb)))
     #f]
    [else
     (define vr (vector-ref rows row))
     (unless vr #f)
     (define cells (vbuffer-row-cells vr))
     (define buf-start (vbuffer-row-buf-start vr))
     (define buf-end   (vbuffer-row-buf-end vr))
     (define gb (vbuffer-gap vb))
     (unless gb #f)
     (define ncols (vbuffer-ncols vb))
     (let loop ([c 0] [bp buf-start] [skip? #f])
       (cond
         [(>= c col)
          ;; Reached target column
          (if (and skip? (> bp buf-start))
              ;; Target is trailing column of wide char → walk back
              ;; to find the byte position of the wide char's first column
              (find-byte-at-col cells buf-start gb c)
              bp)]
         [(>= c ncols) buf-end]
         [(>= bp buf-end) buf-end]
         [else
          (define cl (vector-ref cells c))
          (define ch (cell-ch cl))
          (define cw* (max 1 (char-display-width ch)))
          (define at-trailing?
            (and skip? (char=? ch #\space) (zero? (cell-face-id cl))))
          (if at-trailing?
              (loop (add1 c) bp #f)
              (loop (+ c (if (>= cw* 2) 2 1))
                    (if (>= bp buf-end) bp (gap-next-char-pos gb bp))
                    (>= cw* 2)))]))
    ]))

;; Helper: find the byte-pos of the character at column `col`
;; by walking from buf-start.
(define (find-byte-at-col cells buf-start gb col)
  (let loop ([c 0] [bp buf-start])
    (if (>= c col)
        bp
        (let ([ch (cell-ch (vector-ref cells c))]
              [cw* (max 1 (char-display-width (cell-ch (vector-ref cells c))))])
          (loop (+ c (if (>= cw* 2) 2 1))
                (if (>= bp (gap-length gb)) bp (gap-next-char-pos gb bp)))))))

;; ============================================================
;; Buffer → Screen: vbuffer-byte-pos->xy
;; ============================================================
;;
;; Linear scan through rows to find which row contains the target
;; byte position, then walk columns to find the exact column.

(define (vbuffer-byte-pos->xy vb target-pos)
  (define rows (vbuffer-rows vb))
  (define gb (vbuffer-gap vb))
  (unless gb (values 0 0))
  (define nrows (vbuffer-nrows vb))
  (define ncols (vbuffer-ncols vb))
  (let search ([r 0])
    (cond
      [(>= r nrows) (values (sub1 nrows) (sub1 ncols))]
      [(not (vector-ref rows r)) (search (add1 r))]
      [else
       (define vr (vector-ref rows r))
       (define buf-start (vbuffer-row-buf-start vr))
       (define buf-end   (vbuffer-row-buf-end vr))
       (cond
         [(< target-pos buf-start) (search (add1 r))]
         [(< target-pos buf-end)
          (define cells (vbuffer-row-cells vr))
          (let loop ([c 0] [bp buf-start] [skip? #f])
            (cond
              [(>= bp target-pos) (values r c)]
              [(>= c ncols) (values r (sub1 ncols))]
              [(>= bp buf-end) (values r (sub1 ncols))]
              [else
               (define cl (vector-ref cells c))
               (define ch (cell-ch cl))
               (define cw* (max 1 (char-display-width ch)))
               (define is-wide? (>= cw* 2))
               (define advance (if (>= cw* 2) 2 1))
               (loop (+ c advance)
                     (if (>= bp buf-end) bp (gap-next-char-pos gb bp))
                     is-wide?)]))]
         [(>= r (sub1 nrows)) (values r (sub1 ncols))]
         [else (search (add1 r))])])))

;; ============================================================
;; Comparison — for delta flush (skip unchanged rows)
;; ============================================================

(define (vbuffer-cell-equal? a b)
  (and (char=? (cell-ch a) (cell-ch b))
       (= (cell-face-id a) (cell-face-id b))
       (equal? (cell-attrs a) (cell-attrs b))))

(define (vbuffer-row-changed? vb cache row)
  (define vb-rows (vbuffer-rows vb))
  (cond
    [(or (not cache) (>= row (vbuffer-nrows cache))) #t]
    [else
     (define vr (vector-ref vb-rows row))
     (define cr (vector-ref (vbuffer-rows cache) row))
     (cond [(not vr) (and cr #t)]
           [(not cr) #t]
           [else
            (define vcells (vbuffer-row-cells vr))
            (define ccells (vbuffer-row-cells cr))
            (for/or ([c (in-range (vbuffer-ncols vb))])
              (not (vbuffer-cell-equal?
                    (vector-ref vcells c)
                    (vector-ref ccells c))))])]))

;; ============================================================
;; vbuffer-blit! — copy src vbuffer into dst at (top, left)
;; ============================================================

(define (vbuffer-blit! dst dst-top dst-left src)
  (unless (vbuffer? dst)
    (raise-argument-error 'vbuffer-blit! "vbuffer?" dst))
  (unless (vbuffer? src)
    (raise-argument-error 'vbuffer-blit! "vbuffer?" src))
  (define src-rows (vbuffer-rows src))
  (define dst-rows (vbuffer-rows dst))
  (define dst-ncols (vbuffer-ncols dst))
  (define src-ncols (vbuffer-ncols src))

  (for ([sr (in-range (vbuffer-nrows src))]
        #:when (and (< (+ dst-top sr) (vbuffer-nrows dst))
                    (vector-ref src-rows sr)))
    (define dr (+ dst-top sr))
    (define svr (vector-ref src-rows sr))
    (define new-cells (make-vector dst-ncols (cell #\space #f 0)))
    (define scells (vbuffer-row-cells svr))
    (for ([sc (in-range src-ncols)]
          #:when (< (+ dst-left sc) dst-ncols))
      (vector-set! new-cells (+ dst-left sc) (vector-ref scells sc)))
    (vector-set! dst-rows dr
      (vbuffer-row new-cells
                   (vbuffer-row-buf-start svr)
                   (vbuffer-row-buf-end svr)
                   (vbuffer-row-continued? svr)
                   (vbuffer-row-truncated? svr)
                   (vbuffer-row-display-len svr)))))

;; ============================================================
;; Serialization
;; ============================================================

(define (vbuffer-row->string vb row)
  (unless (and (>= row 0) (< row (vbuffer-nrows vb)))
    (raise-argument-error 'vbuffer-row->string
                          (format "row in [0, ~a)" (vbuffer-nrows vb)) row))
  (define vr (vector-ref (vbuffer-rows vb) row))
  (if vr
      (list->string
       (for/list ([c (in-range (vbuffer-ncols vb))])
         (cell-ch (vector-ref (vbuffer-row-cells vr) c))))
      ""))

;; ============================================================
;; Iteration
;; ============================================================

(define (in-vbuffer-rows vb)
  (in-vector (vbuffer-rows vb)))
