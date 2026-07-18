#lang racket

;; display/debug/vbuffer-debug.rkt — VBuffer debug S-expression output
;;
;; ============================================================================
;; Produces S-expression snapshots of vbuffer state for offline analysis
;; and debugging of the render pipeline.
;; ============================================================================

(require "../vbuffer.rkt")

(provide
 vbuffer-debug-summary     ;; vb → "(vbuffer Nrows×Ncols ...)"
 vbuffer-debug-row         ;; vb row → "(row R [Bstart Bend] "chfid...")"
 vbuffer-debug-contents    ;; vb → string with row annotations
 vbuffer-debug-xy-check)   ;; vb → cross-check xy↔byte-pos consistency

;; ============================================================
;; vbuffer-debug-summary — top-level overview
;; ============================================================

(define (vbuffer-debug-summary vb)
  (define nrows (vbuffer-nrows vb))
  (define ncols (vbuffer-ncols vb))
  (define filled (for/sum ([r (in-range nrows)])
                   (if (vector-ref (vbuffer-rows vb) r) 1 0)))
  (format "(vbuffer ~a×~a (filled ~a))" nrows ncols filled))

;; ============================================================
;; vbuffer-debug-row — one row as S-expression
;; ============================================================

(define (vbuffer-debug-row vb row)
  (unless (and (>= row 0) (< row (vbuffer-nrows vb)))
    (raise-argument-error 'vbuffer-debug-row
                          (format "row in [0, ~a)" (vbuffer-nrows vb)) row))
  (define vr (vector-ref (vbuffer-rows vb) row))
  (if vr
      (let* ([cells     (vbuffer-row-cells vr)]
             [buf-start (vbuffer-row-buf-start vr)]
             [buf-end   (vbuffer-row-buf-end vr)]
             [continued? (vbuffer-row-continued? vr)]
             [truncated? (vbuffer-row-truncated? vr)]
             [dlen       (vbuffer-row-display-len vr)]
             [ncols      (vbuffer-ncols vb)]
             [chars
              (for/list ([c (in-range ncols)])
                (let* ([cl (vector-ref cells c)]
                       [ch (cell-ch cl)]
                       [fid (cell-face-id cl)])
                  (format "~a~a"
                          (if (char=? ch #\space) #\_ ch)
                          (if (zero? fid) "" (format "~a" fid)))))])
        (format "(row ~a [~a ~a) ~a~a dlen=~a \"~a\")"
                row buf-start buf-end
                (if continued? "CONT " "")
                (if truncated? "TRUNC " "")
                dlen
                (string-join chars "")))
      (format "(row ~a EMPTY)" row)))

;; ============================================================
;; vbuffer-debug-contents — all rows with annotations
;; ============================================================

(define (vbuffer-debug-contents vb)
  (define nrows (vbuffer-nrows vb))
  (define parts
    (for/list ([r (in-range nrows)])
      (vbuffer-debug-row vb r)))
  (string-join parts "\n"))

;; ============================================================
;; vbuffer-debug-xy-check — validate screen↔buffer mapping
;; ============================================================

(define (vbuffer-debug-xy-check vb)
  (define nrows (vbuffer-nrows vb))
  (define ncols (vbuffer-ncols vb))
  (define issues '())
  (define ok-count 0)
  (for ([r (in-range nrows)]
        #:when (vector-ref (vbuffer-rows vb) r))
    (define vr (vector-ref (vbuffer-rows vb) r))
    (define buf-start (vbuffer-row-buf-start vr))
    (define buf-end (vbuffer-row-buf-end vr))
    ;; byte-pos at row start should map back to this row
    (let-values ([(back-r back-c) (vbuffer-byte-pos->xy vb buf-start)])
      (unless (= back-r r)
        (set! issues
              (cons (format "(mismatch row~a-start@~a -> (~a ~a))"
                            r buf-start back-r back-c)
                    issues))))
    ;; byte-pos at row end-1 should be in this row
    (when (> buf-end buf-start)
      (let-values ([(back-r back-c) (vbuffer-byte-pos->xy vb (sub1 buf-end))])
        (unless (= back-r r)
          (set! issues
                (cons (format "(mismatch row~a-end-1@~a -> (~a ~a))"
                              r (sub1 buf-end) back-r back-c)
                      issues)))))
    ;; sample columns map to valid bytes in this row
    (for ([c (in-range (min 10 ncols))])
      (define bp (vbuffer-xy->byte-pos vb r c))
      (when bp
        (unless (and (>= bp buf-start) (<= bp buf-end))
          (set! issues
                (cons (format "(out-of-range row~a col~a -> byte~a not in [~a ~a])"
                              r c bp buf-start buf-end)
                      issues)))
        (set! ok-count (add1 ok-count)))))
  (format "(vbuffer-xy-check ~a ~a rows=~a cols=~a samples-ok=~a)"
          (if (null? issues) "PASS" "FAIL")
          (if (null? issues) ""
              (format "~a" (string-join (reverse issues) " ")))
          nrows ncols ok-count))
