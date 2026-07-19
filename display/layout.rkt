#lang racket

;; display/layout.rkt — Pure layout: buffer bytes → visual-lines
;;
;; ============================================================================
;; Takes a gap-buffer + viewport parameters → produces a Layout value.
;; The Layout is an immutable description of what should appear on screen.
;; Zero side effects.  All parameters passed explicitly.
;;
;; ============================================================================
;; Computation Only — pure functions, no mutation
;; ============================================================================
;;
;;   compute-layout    — gap-buffer × params → layout
;;   truncate-lines    — one visual-line per logical line
;;   wrap-lines        — split lines at column boundary
;;   layout-query-pos  — screen (row,col) → buffer byte-pos
;;   pos->row-col      — buffer byte-pos → screen (row,col)
;;   calc-scroll       — keep point visible (pure result)
;;
;; ============================================================================
;; Dependencies: kernel/data/gap, kernel/data/query, kernel/data/char-width
;; ============================================================================

(require "../kernel/data/gap.rkt"
         "../kernel/data/query.rkt"
         "../kernel/data/char-width.rkt")

(provide
 ;; ── layout ──
 layout? compute-layout
 layout-lines layout-cursor-row layout-cursor-col
 layout-start-pos layout-max-rows layout-max-cols
 layout-wrap-mode layout-left-col

 ;; ── visual-line ──
 visual-line? visual-line-buf-pos visual-line-content
 visual-line-continued? visual-line-truncated?
 visual-line-display-len visual-line-end-buf-pos

 ;; ── line generation (for direct use) ──
 truncate-lines wrap-lines visual-line-lines

 ;; ── position queries ──
 layout-query-pos      ;; gb layout row col → (or/c #f byte-pos)
 pos->row-col          ;; gb start target → (values row col)

 ;; ── scroll calculation (pure — no side effects) ──
 calc-scroll)          ;; gb pt-pos start rows cols hscroll wrap → (values new-start new-hscroll)

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
  ;; Contracts validated by called functions.
  (define vlines (visual-line-lines gb start rows cols
                                    #:wrap-mode wrap #:left-col left))
  (define-values (cr cc)
    (if buf-point-pos
        (let-values ([(row col) (pos->row-col gb start buf-point-pos)])
          ;; Screen column matches what truncate-lines actually rendered.
          ;; Row content starts at the char-boundary snap of left-col,
          ;; not at raw left-col.  Subtracting raw left-col can go off
          ;; by one when a wide char straddles the hscroll boundary.
          (define screen-col
            (cond [(and (eq? wrap 'none) (< row (length vlines)))
                   (define vl (list-ref vlines row))
                   (if (< buf-point-pos (visual-line-buf-pos vl))
                       0
                       (gap-display-width gb (visual-line-buf-pos vl)
                                          buf-point-pos))]
                  [else (- col left)]))
          (values row (max 0 (min (sub1 cols) screen-col))))
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
  (unless (and (gap-valid-pos? gb start-pos)
               (exact-positive-integer? max-rows)
               (exact-positive-integer? max-cols)
               (exact-nonnegative-integer? left-col))
    (raise-argument-error 'truncate-lines
                          "valid start, positive rows/cols, non-negative left-col"
                          (list start-pos max-rows max-cols left-col)))
  (define len (gap-length gb))
  (define (nl? b) (= b #x0A))
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

;; ============================================================
;; wrap-lines — split logical lines at max-cols
;; ============================================================

(define (wrap-lines gb start-pos max-rows max-cols)
  (unless (and (gap-valid-pos? gb start-pos)
               (exact-positive-integer? max-rows)
               (exact-positive-integer? max-cols))
    (raise-argument-error 'wrap-lines
                          "valid start, positive rows and cols"
                          (list start-pos max-rows max-cols)))
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

(define (layout-query-pos gb ly row col)
  ;; Screen (row, col) — 0-based, relative to layout — → buffer byte-pos.
  ;; Returns the byte position of the character at that screen cell.
  (unless (layout? ly)
    (raise-argument-error 'layout-query-pos "layout?" ly))
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
            [target-col  col]
            [end-buf-pos (visual-line-end-buf-pos vl)])
       (if (>= target-col (gap-display-width gb buf-pos end-buf-pos))
           ;; Past end of this visual line → clamp to end of line
           (if (and (> end-buf-pos buf-pos)
                    (= (gap-byte-ref gb (sub1 end-buf-pos)) #x0A))
               (sub1 end-buf-pos)
               end-buf-pos)
           (scan-display-width gb buf-pos end-buf-pos target-col)))]))

;; ============================================================
;; pos->row-col — buffer byte-pos → screen (row, col)
;; ============================================================

(define (pos->row-col gb start-pos target-pos)
  (unless (and (gap-valid-pos? gb start-pos)
               (gap-valid-pos? gb target-pos))
    (raise-argument-error 'pos->row-col
                          "valid positions in gap" (list start-pos target-pos)))
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
;; Scroll — viewport = [start-line, start-line+max-rows) × [hscroll, hscroll+max-cols)
;; ============================================================
;;
;; Point outside viewport → move viewport to bring point back in.
;; scroll-margin shrinks the safe zone inward.
;;
;;   Above: new-start-line = max(0, pt-line - margin)
;;   Below: new-start-line = pt-line - max-rows + margin + 1
;;   Left:  new-hscroll     = pt-col
;;   Right: new-hscroll     = pt-col - max-cols + 1

(define scroll-margin 0)

(define (calc-scroll gb pt-pos start-pos max-rows max-cols hscroll wrap-mode)
  (define pt-line   (line-of-pos gb pt-pos))
  (define start-line (line-of-pos gb start-pos))

  ;; ── Vertical ──
  (define m (if (eq? wrap-mode 'none) scroll-margin 0))
  (define safe-top (+ start-line m))
  (define safe-bot (+ start-line max-rows (- m)))

  (define new-start-line
    (cond [(< pt-line safe-top) (max 0 (- pt-line m))]
          [(>= pt-line safe-bot) (+ pt-line (- max-rows) m 1)]
          [else #f]))
  (define new-start (and new-start-line (nth-line-start gb new-start-line)))

  ;; ── Horizontal ──
  (define base-h (if new-start 0 hscroll))
  (define new-hscroll
    (if (eq? wrap-mode 'none)
        (let* ([bol (gap-scan-byte gb pt-pos 'backward (λ (b) (= b #x0A)))]
               [line-start (if (>= bol 0) (add1 bol) 0)]
               [pt-col (gap-display-width gb line-start pt-pos)])
          (cond [(< pt-col base-h) pt-col]
                [(>= pt-col (+ base-h max-cols))
                 (hscroll-char-boundary gb line-start pt-pos
                                        (max 0 (- pt-col max-cols -1)))]
                [else base-h]))
        base-h))

  (values (or new-start start-pos) new-hscroll))

;; ============================================================
;; Line helpers — absolute line numbers (0-based, \n = line boundary)
;; ============================================================

(define (line-of-pos gb pos)
  ;; Number of \n before pos = absolute line number of pos.
  (define (nl? b) (= b #x0A))
  (let loop ([p 0] [line 0])
    (if (>= p pos) line
        (let ([nl (gap-scan-byte gb p 'forward nl?)])
          (if (or (< nl 0) (>= nl pos)) line
              (loop (add1 nl) (add1 line)))))))

(define (nth-line-start gb n)
  ;; Byte position of the start of line N (0-based).
  (if (zero? n) 0
      (let loop ([p 0] [remaining n])
        (let ([nl (gap-scan-byte gb p 'forward (λ (b) (= b #x0A)))])
          (if (>= nl (gap-length gb)) nl
              (if (zero? (sub1 remaining)) (add1 nl)
                  (loop (add1 nl) (sub1 remaining))))))))

;; ============================================================
;; Horizontal helpers
;; ============================================================

(define (hscroll-char-boundary gb line-start pt-pos raw)
  ;; Smallest display column ≥ RAW that falls on a char boundary.
  (define seg (scan-display-width gb line-start pt-pos raw))
  (define w (gap-display-width gb line-start seg))
  (if (or (>= w raw) (>= seg pt-pos))
      w
      (+ w (max 0 (char-display-width (gap-char gb seg))))))
