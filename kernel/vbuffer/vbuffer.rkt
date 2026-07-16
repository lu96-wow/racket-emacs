#lang racket

;; kernel/vbuffer/vbuffer.rkt — Virtual screen buffer (offscreen cell grid)
;;
;; rows × cols matrix of cells.  Each cell: char + attrs + face-id.
;; Used as the "desired matrix" for double-buffered rendering.
;; Zero dependencies.

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

 ;; serialization
 vbuffer-row->string vbuffer->lines)

(struct cell (ch [attrs #:mutable] [face-id #:mutable]) #:transparent)

(struct vbuffer (rows cols cells) #:transparent)

(define (make-vbuffer rows cols)
  (define len (* rows cols))
  (vbuffer rows cols
    (for/vector ([i (in-range len)])
      (cell #\space #f 0))))

;; Internal: row,col → index
(define (vb-index vb row col)
  (+ (* row (vbuffer-cols vb)) col))

(define (vb-valid? vb row col)
  (and (>= row 0) (< row (vbuffer-rows vb))
       (>= col 0) (< col (vbuffer-cols vb))))

(define (vb-ref vb row col)
  (vector-ref (vbuffer-cells vb) (vb-index vb row col)))

(define (vb-set! vb row col v)
  (vector-set! (vbuffer-cells vb) (vb-index vb row col) v))

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

(define (vbuffer-row->string vb row)
  (define cols (vbuffer-cols vb))
  (list->string
   (for/list ([c (in-range cols)])
     (cell-ch (vb-ref vb row c)))))

(define (vbuffer->lines vb)
  (for/list ([r (in-range (vbuffer-rows vb))])
    (vbuffer-row->string vb r)))

(define (vbuffer-blit! dst dst-top dst-left src)
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
