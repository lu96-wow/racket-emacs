#lang racket

;; display/vbuffer.rkt — Virtual screen buffer (offscreen cell grid)
;;
;; A rows×cols matrix of cells.  Each cell: char + attrs + face-id.
;; Used as the "desired matrix" for double-buffered rendering:
;;   1. render fills a vbuffer (layout + faces → vbuffer)
;;   2. flush diffs it against the cached previous frame
;;
;; Pure data structure.  Zero dependencies on kernel or platform.
;; Mutations are in-place (vector-set!) for performance.

(provide
 ;; cell
 cell? cell-ch cell-attrs cell-face-id

 ;; vbuffer
 vbuffer? vbuffer-rows vbuffer-cols vbuffer-cells
 make-vbuffer

 ;; mutation
 vbuffer-clear!
 vbuffer-put-char! vbuffer-put-string!
 vbuffer-fill! vbuffer-blit!

 ;; comparison (for delta flushing)
 vbuffer-row-changed? vbuffer-cell-equal?

 ;; serialization
 vbuffer-row->string vbuffer->lines)

;; ============================================================
;; Cell — one screen position
;; ============================================================

(struct cell
  (ch           ; char? — character to display
   [attrs #:mutable]     ; (or/c #f symbol? (listof symbol?)) — bold, reverse, etc.
   [face-id #:mutable])  ; exact-nonnegative-integer? — index into face table
  #:transparent)

;; ============================================================
;; VBuffer — rows × cols matrix
;; ============================================================

(struct vbuffer
  (rows   ; exact-positive-integer?
   cols   ; exact-positive-integer?
   cells) ; (vectorof cell?) — row-major, length = rows*cols
  #:transparent)

(define (make-vbuffer rows cols)
  (define len (* rows cols))
  (vbuffer rows cols
    (for/vector ([i (in-range len)])
      (cell #\space #f 0))))

;; ============================================================
;; Internal helpers
;; ============================================================

(define (vb-index vb row col)
  (+ (* row (vbuffer-cols vb)) col))

(define (vb-valid? vb row col)
  (and (>= row 0) (< row (vbuffer-rows vb))
       (>= col 0) (< col (vbuffer-cols vb))))

(define (vb-ref vb row col)
  (vector-ref (vbuffer-cells vb) (vb-index vb row col)))

(define (vb-set! vb row col v)
  (vector-set! (vbuffer-cells vb) (vb-index vb row col) v))

;; ============================================================
;; Mutation
;; ============================================================

(define (vbuffer-clear! vb)
  (for ([i (in-range (vector-length (vbuffer-cells vb)))])
    (vector-set! (vbuffer-cells vb) i (cell #\space #f 0))))

(define (vbuffer-put-char! vb row col ch [attrs #f] #:face-id [fid 0])
  (when (vb-valid? vb row col)
    (vb-set! vb row col (cell ch attrs fid))))

(define (vbuffer-put-string! vb row col str [attrs #f] #:face-id [fid 0])
  (define max-col (vbuffer-cols vb))
  (for ([i (in-range (string-length str))] [ch (in-string str)])
    (define c (+ col i))
    (when (and (>= c 0) (< c max-col))
      (vb-set! vb row c (cell ch attrs fid)))))

(define (vbuffer-fill! vb row col len ch [attrs #f] #:face-id [fid 0])
  (for ([i (in-range len)])
    (define c (+ col i))
    (when (and (>= c 0) (< c (vbuffer-cols vb)))
      (vb-set! vb row c (cell ch attrs fid)))))

(define (vbuffer-blit! dst dst-top dst-left src)
  ;; Copy src vbuffer into dst at (dst-top, dst-left).
  (define src-rows (vbuffer-rows src))
  (define src-cols (vbuffer-cols src))
  (define dst-cols (vbuffer-cols dst))
  (define dst-cells (vbuffer-cells dst))
  (define dst-end-row (min (vbuffer-rows dst) (+ dst-top src-rows)))
  (define dst-end-col (min dst-cols (+ dst-left src-cols)))
  (for ([sr (in-range src-rows)]
        #:when (< (+ dst-top sr) dst-end-row))
    (define dr (+ dst-top sr))
    (define row-offset (* dr dst-cols))
    (for ([sc (in-range src-cols)]
          #:when (< (+ dst-left sc) dst-end-col))
      (vector-set! dst-cells (+ row-offset dst-left sc)
        (vector-ref (vbuffer-cells src) (+ (* sr src-cols) sc))))))

;; ============================================================
;; Comparison — for delta flush (skip unchanged rows)
;; ============================================================

(define (vbuffer-cell-equal? a b)
  (and (char=? (cell-ch a) (cell-ch b))
       (= (cell-face-id a) (cell-face-id b))
       (equal? (cell-attrs a) (cell-attrs b))))

(define (vbuffer-row-changed? vb cache row)
  ;; True if any cell in `row` differs between vb and cache.
  ;; cache must have same cols as vb.
  (define cols (vbuffer-cols vb))
  (when (and cache (= cols (vbuffer-cols cache)))
    (define start (* row cols))
    (for/or ([c (in-range cols)])
      (not (vbuffer-cell-equal?
            (vector-ref (vbuffer-cells vb) (+ start c))
            (vector-ref (vbuffer-cells cache) (+ start c)))))))

;; ============================================================
;; Serialization
;; ============================================================

(define (vbuffer-row->string vb row)
  (define cols (vbuffer-cols vb))
  (list->string
   (for/list ([c (in-range cols)])
     (cell-ch (vb-ref vb row c)))))

(define (vbuffer->lines vb)
  (for/list ([r (in-range (vbuffer-rows vb))])
    (vbuffer-row->string vb r)))
