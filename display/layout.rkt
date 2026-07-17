#lang racket

;; display/layout.rkt — Pure layout: buffer bytes → visual-lines
;;
;; Takes a gap-buffer + viewport parameters → produces a Layout value.
;; The Layout is an immutable description of what should appear on screen.
;; Zero side effects.  All parameters (wrap-mode, hscroll) passed explicitly.
;;
;; Consumed by render.rkt to fill a vbuffer.
;;
;; Dependencies: kernel/data/gap, kernel/data/query, display/char-width

(require "../kernel/data/gap.rkt"
         "../kernel/data/query.rkt"
         "char-width.rkt")

(provide
 ;; layout
 layout? compute-layout
 layout-lines layout-cursor-row layout-cursor-col
 layout-start-pos layout-max-rows layout-max-cols
 layout-wrap-mode layout-left-col

 ;; visual-line
 visual-line? visual-line-buf-pos visual-line-content
 visual-line-continued? visual-line-truncated?
 visual-line-display-len visual-line-end-buf-pos

 ;; line generation (for direct use)
 truncate-lines wrap-lines visual-line-lines

 ;; position queries
 layout-query-pos      ;; gb layout row col → (or/c #f byte-pos)
 pos->row-col          ;; gb start target → (values row col)

 ;; scroll calculation (pure — no side effects)
 calc-scroll           ;; gb pt-pos start rows cols hscroll wrap → (values new-start new-hscroll)
 )

;; ============================================================
;; Layout — immutable snapshot of the screen
;; ============================================================

(struct layout
  (lines        ; (listof visual-line?) — ordered top-to-bottom
   cursor-row   ; (or/c exact-nonnegative-integer? #f)
   cursor-col   ; (or/c exact-nonnegative-integer? #f)
   start-pos    ; byte-pos — first visible buffer byte
   max-rows     ; exact-positive-integer?
   max-cols     ; exact-positive-integer?
   wrap-mode    ; (or/c 'none 'char)
   left-col)    ; exact-nonnegative-integer? — hscroll
  #:transparent)

(define (compute-layout gb buf-point-pos
                        #:start-pos [start 0]
                        #:max-rows  [rows 24]
                        #:max-cols  [cols 80]
                        #:wrap-mode [wrap 'none]
                        #:left-col  [left 0])
  ;; Pure: gap-buffer × params → layout.
  (define vlines (visual-line-lines gb start rows cols
                                    #:wrap-mode wrap #:left-col left))
  (define-values (cr cc)
    (if buf-point-pos
        (pos->row-col gb start buf-point-pos)
        (values #f #f)))
  (layout vlines cr cc start rows cols wrap left))

;; ============================================================
;; visual-line — one displayed row
;; ============================================================

(struct visual-line
  (buf-pos     ; byte-pos — first buffer byte of this line
   content     ; string — displayed characters
   continued?  ; boolean? — continuation of wrapped line?
   truncated?  ; boolean? — '$' at end?
   display-len ; exact-nonnegative-integer? — display columns
   end-buf-pos ; byte-pos — first byte after this line
   )
  #:transparent)

;; ============================================================
;; truncate-lines — one visual-line per logical line
;; ============================================================

(define (truncate-lines gb start-pos max-rows max-cols left-col)
  (define len (gap-length gb))
  (define (nl? b) (= b #x0A))
  (define lines
    (let loop ([buf-pos start-pos] [row 0] [acc '()])
      (if (or (>= row max-rows) (>= buf-pos len))
          (reverse acc)
          (let* ([line-end    (gap-scan-byte gb buf-pos 'forward nl?)]
                 [line-limit  (if (< line-end len) line-end len)]
                 [full-width  (gap-display-width gb buf-pos line-limit)]
                 [trunc?      (> full-width (+ left-col max-cols))]
                 [reserve-$   (if trunc? 1 0)]
                 [seg-start   (if (> left-col 0)
                                  (scan-display-width gb buf-pos line-limit left-col)
                                  buf-pos)]
                 [actual-left (gap-display-width gb buf-pos seg-start)]
                 [extra-cols  (- left-col actual-left)]
                 [cols-left   (max 1 (- max-cols reserve-$ extra-cols))]
                 [seg-end     (min line-limit
                                   (scan-display-width gb seg-start line-limit cols-left))]
                 [content     (gap-substring gb seg-start seg-end)]
                 [display-len (for/sum ([ch (in-string content)])
                                (max 0 (char-display-width ch)))]
                 [next-buf-pos (if (< line-end len) (add1 line-end) len)])
            (loop next-buf-pos
                  (add1 row)
                  (cons (visual-line seg-start content #f trunc? display-len
                                     next-buf-pos)
                        acc))))))
  ;; If buffer ends with newline and we have room, add an empty visual line
  (if (and (< (length lines) max-rows) (> len 0)
           (= (gap-byte-ref gb (sub1 len)) #x0A))
      (append lines (list (visual-line len "" #f #f 0 len)))
      lines))

;; ============================================================
;; wrap-lines — split logical lines at max-cols
;; ============================================================

(define (wrap-lines gb start-pos max-rows max-cols)
  (define len (gap-length gb))
  (define (nl? b) (= b #x0A))
  (let buffer-loop ([buf-pos start-pos] [row 0] [acc '()])
    (if (or (>= row max-rows) (>= buf-pos len))
        (reverse acc)
        (let* ([line-end   (gap-scan-byte gb buf-pos 'forward nl?)]
               [line-limit (if (< line-end len) line-end len)])
          (let visual-loop ([seg-pos buf-pos] [vrow row] [seg-acc acc] [cont? #f])
            (define seg-end     (scan-display-width gb seg-pos line-limit max-cols))
            (define content     (gap-substring gb seg-pos seg-end))
            (define display-len (for/sum ([ch (in-string content)])
                                  (max 0 (char-display-width ch))))
            (define vl (visual-line seg-pos content cont? #f display-len seg-end))
            (cond
              [(>= seg-end line-limit)
               (buffer-loop (if (< line-end len) (add1 line-end) len)
                            (add1 vrow) (cons vl seg-acc))]
              [(>= (add1 vrow) max-rows)
               (reverse (cons vl seg-acc))]
              [else
               (visual-loop seg-end (add1 vrow) (cons vl seg-acc) #t)]))))))

;; ============================================================
;; visual-line-lines — dispatcher
;; ============================================================

(define (visual-line-lines gb start-pos max-rows max-cols
                           #:wrap-mode [wrap 'none]
                           #:left-col  [left 0])
  (if (eq? wrap 'none)
      (truncate-lines gb start-pos max-rows max-cols left)
      (wrap-lines gb start-pos max-rows max-cols)))

;; ============================================================
;; layout-query-pos — screen (row, col) → buffer byte-pos
;; ============================================================
;; For mouse/input: user clicked at screen position → which buffer byte?

(define (layout-query-pos gb ly row col)
  ;; Screen (row, col) — 0-based, relative to layout — → buffer byte-pos.
  ;; Returns the byte position of the character at that screen cell.
  ;; Clicks past end-of-buffer return buffer-end (below the last line)
  ;; or end-of-line (beyond the last column).
  (define lines (layout-lines ly))
  (define buf-len (gap-length gb))
  (cond
    ;; Below all content → buffer end
    [(>= row (length lines))
     (if (null? lines)
         (layout-start-pos ly)
         (visual-line-end-buf-pos (last lines)))]
    [else
     (let* ([vl          (list-ref lines row)]
            [buf-pos     (visual-line-buf-pos vl)]
            [left-col    (layout-left-col ly)]
            [target-col  (+ left-col col)]
            [end-buf-pos (visual-line-end-buf-pos vl)])
       (if (>= target-col (gap-display-width gb buf-pos end-buf-pos))
           ;; Past end of this visual line → clamp to line end
           end-buf-pos
           (scan-display-width gb buf-pos end-buf-pos target-col)))]))

;; ============================================================
;; pos->row-col — buffer byte-pos → screen (row, col)
;; ============================================================

(define (pos->row-col gb start-pos target-pos)
  (define len (gap-length gb))
  (define (nl? b) (= b #x0A))
  (let loop ([row 0] [pos start-pos])
    (cond [(>= pos target-pos) (values row (gap-display-width gb pos target-pos))]
          [(>= pos len)        (values row (gap-display-width gb pos target-pos))]
          [else (define nl (gap-scan-byte gb pos 'forward nl?))
                (cond [(or (>= nl target-pos) (>= nl len))
                       (values row (gap-display-width gb pos target-pos))]
                      [else (loop (add1 row) (add1 nl))])])))

;; ============================================================
;; Scroll calculation — keep point visible in viewport
;; ============================================================

(define (calc-scroll gb pt-pos start-pos max-rows max-cols
                     hscroll wrap-mode)
  ;; Pure: determines the scroll position that brings pt-pos into view.
  ;; Returns (values new-start new-hscroll) — both are byte-pos and
  ;; column respectively.  The caller applies them to leaf markers.
  (define len (gap-length gb))

  ;; 1. Calculate the last visible buffer position
  (define last-visible-pos
    (if (eq? wrap-mode 'none)
        (end-of-physical-lines gb start-pos max-rows)
        (let ([vlines (visual-line-lines gb start-pos max-rows max-cols
                                         #:wrap-mode 'char)])
          (if (null? vlines)
              start-pos
              (visual-line-end-buf-pos (last vlines))))))

  ;; 2. Vertical: is point before the visible region?
  (define new-start
    (cond [(< pt-pos start-pos)
           ;; Point is above — scroll up so point is on first line
           (let ([nl (gap-scan-byte gb pt-pos 'backward (λ (b) (= b #x0A)))])
             (if (>= nl 0) (add1 nl) 0))]
          [(>= pt-pos last-visible-pos)
           ;; Point is below — scroll down so point is 1/3 from bottom
           (define target-lines (max 1 (quotient (* max-rows 2) 3)))
           (beginning-of-nth-prev-line gb pt-pos target-lines)]
          [else
           ;; Point is visible — keep current scroll
           #f]))

  ;; 3. Horizontal: is point's column off-screen? (truncate only)
  (define new-hscroll
    (if (eq? wrap-mode 'none)
        (let* ([bol (gap-scan-byte gb pt-pos 'backward (λ (b) (= b #x0A)))]
               [line-start (if (>= bol 0) (add1 bol) 0)]
               [pt-col (gap-display-width gb line-start pt-pos)])
          (cond [(< pt-col hscroll)
                 ;; Point left of visible area
                 pt-col]
                [(>= pt-col (+ hscroll max-cols))
                 ;; Point right of visible area
                 (max 0 (- pt-col max-cols -1))]
                [else #f]))
        #f))  ; no hscroll in wrap mode

  (values (or new-start start-pos)
          (cond [new-start
                 ;; Vertical scroll happened — reset hscroll.
                 ;; Prevents long-line hscroll from leaking into
                 ;; shorter lines above/below.
                 (if (> hscroll 0) 0 hscroll)]
                [new-hscroll new-hscroll]
                [else hscroll])))

;; ============================================================
;; Scroll helpers (internal)
;; ============================================================

(define (end-of-physical-lines gb start n)
  ;; Return the byte-pos after advancing past N newlines from start.
  (define len (gap-length gb))
  (define (nl? b) (= b #x0A))
  (let loop ([pos start] [remaining n])
    (if (or (zero? remaining) (>= pos len))
        pos
        (let ([nl (gap-scan-byte gb pos 'forward nl?)])
          (if (>= nl len)
              len
              (loop (add1 nl) (sub1 remaining)))))))

(define (beginning-of-nth-prev-line gb pos n)
  ;; Return the byte-pos of the start of the Nth previous line.
  (define (nl? b) (= b #x0A))
  (let loop ([p pos] [remaining n])
    (if (<= p 0)
        0
        (let ([nl (gap-scan-byte gb (max 0 (sub1 p)) 'backward nl?)])
          (if (< nl 0)
              0
              (if (zero? remaining)
                  (add1 nl)
                  (loop nl (sub1 remaining))))))))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit
           "../kernel/data/text.rkt")

  (define (make-gb str)
    (text-gap (make-text str)))

  (test-case "truncate-lines simple"
    (let* ([gb (make-gb "line one\nline two\n")]
           [ls (truncate-lines gb 0 10 20 0)])
      (check-equal? (length ls) 3)
      (check-equal? (visual-line-content (car ls)) "line one")
      (check-equal? (visual-line-content (cadr ls)) "line two")
      (check-equal? (visual-line-content (caddr ls)) "")))

  (test-case "truncate-lines with truncation"
    (let* ([gb (make-gb "this line is much too long for the screen\n")]
           [ls (truncate-lines gb 0 10 14 0)])
      (check-equal? (length ls) 1)
      (check-equal? (visual-line-content (car ls)) "this line is m")
      (check-true (visual-line-truncated? (car ls)))))

  (test-case "wrap-lines"
    (let* ([gb (make-gb "hello world this is text\n")]
           [ls (wrap-lines gb 0 10 10)])
      (check (>= (length ls) 2))
      (check-equal? (visual-line-content (car ls)) "hello worl")
      (check-true (visual-line-continued? (cadr ls)))))

  (test-case "compute-layout with cursor"
    (let* ([gb (make-gb "line one\nline two\n")]
           [ly (compute-layout gb 7  ;; point at byte 7
                               #:max-rows 10 #:max-cols 20)])
      (check-equal? (length (layout-lines ly)) 3)
      (check-equal? (layout-cursor-row ly) 1)
      (check-equal? (layout-cursor-col ly) 0)))

  (test-case "layout-query-pos — mouse click → buffer-pos"
    (let* ([gb (make-gb "hello world\n")]
           [ly (compute-layout gb 0 #:max-rows 5 #:max-cols 20)])
      ;; Click row 0, col 6 → should point at 'w' → byte 6
      (check-equal? (layout-query-pos gb ly 0 6) 6)
      ;; Click row 1 → empty trailing line → byte 12 (end of buffer)
      (check-equal? (layout-query-pos gb ly 1 0) 12))
    ;; Click past end of content → buffer end
    (let* ([gb (make-gb "abc")]
           [ly (compute-layout gb 0 #:max-rows 5 #:max-cols 20)])
      ;; Only 1 line of content, click row 3 → buffer end
      (check-equal? (layout-query-pos gb ly 3 0) 3))
    ;; Click past end of line → clamped to line end
    (let* ([gb (make-gb "ab\n")]
           [ly (compute-layout gb 0 #:max-rows 5 #:max-cols 5)])
      ;; Row 0, col 4 (past "ab") → byte 2 (end of first line)
      (check-equal? (layout-query-pos gb ly 0 4) 2)))

  (test-case "pos->row-col"
    (let* ([gb (make-gb "ab\ncd\n")]
           [start 0])
      (let-values ([(r c) (pos->row-col gb start 0)])
        (check-equal? r 0) (check-equal? c 0))
      (let-values ([(r c) (pos->row-col gb start 4)])
        (check-equal? r 1) (check-equal? c 1))))

  (test-case "calc-scroll — point visible, no scroll"
    (let* ([gb (make-gb "line1\nline2\nline3\nline4\nline5\n")])
      (let-values ([(s h) (calc-scroll gb 0 0 3 80 0 'none)])
        ;; Point at 0, start at 0, 3-row viewport → point IS visible
        (check-equal? s 0)
        (check-equal? h 0))))

  (test-case "calc-scroll — point below viewport (scroll forward)"
    (let* ([gb (make-gb "a\nb\nc\nd\ne\nf\ng\nh\n")])
      ;; Point at byte 14 (after "g\n"), start at 0, 3 rows
      ;; Lines: a b c visible, point at g → scroll
      (let-values ([(s h) (calc-scroll gb 14 0 3 80 0 'none)])
        (check-true (> s 0) "should scroll forward"))))

  (test-case "calc-scroll — point above viewport (scroll backward)"
    (let* ([gb (make-gb "a\nb\nc\nd\ne\n")])
      ;; start at byte 6 (d\n), point at byte 2 (b\n) → scroll back
      (let-values ([(s h) (calc-scroll gb 2 6 3 80 0 'none)])
        (check-true (< s 6) "should scroll backward")
        (check-true (<= s 2) "point should be visible after scroll"))))

  (test-case "calc-scroll — horizontal scroll"
    (let* ([gb (make-gb "abcdefghijklmnopqrstuvwxyz")])
      ;; 26 chars, viewport 10 cols, hscroll=0, point at 25
      (let-values ([(s h) (calc-scroll gb 25 0 1 10 0 'none)])
        (check-equal? s 0 "no vertical scroll")
        (check-true (> h 0) "should hscroll right")))
    (let* ([gb (make-gb "abcdefghijklmnopqrstuvwxyz")])
      ;; hscroll=20, point at 3 (left of view) → scroll left
      (let-values ([(s h) (calc-scroll gb 3 0 1 10 20 'none)])
        (check-equal? s 0)
        (check-true (< h 20) "should hscroll left"))))
)
