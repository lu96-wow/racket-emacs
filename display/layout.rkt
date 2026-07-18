#lang racket

;; display/layout.rkt — Pure layout: buffer bytes → visual-lines
;;
;; Takes a gap-buffer + viewport parameters → produces a Layout value.
;; The Layout is an immutable description of what should appear on screen.
;; Zero side effects.  All parameters (wrap-mode, hscroll) passed explicitly.
;;
;; Consumed by render.rkt to fill a vbuffer.
;;
;; Dependencies: kernel/data/gap, kernel/data/query, kernel/data/char-width

(require "../kernel/data/gap.rkt"
         "../kernel/data/query.rkt"
         "../kernel/data/char-width.rkt")

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
        (let-values ([(row col) (pos->row-col gb start buf-point-pos)])
          ;; Screen column must match what truncate-lines actually
          ;; rendered for this row: the row content starts at the
          ;; char-boundary snap of left-col (visual-line-buf-pos), not
          ;; at raw left-col.  Subtracting raw left-col goes off by one
          ;; when a wide char straddles the hscroll boundary, parking
          ;; the cursor on the trailing cell of a wide char.
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
                                (max 0 (char-display-width ch)))] ; respects cjk-ambiguous-width
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
            [target-col  col]  ;; col is screen-local; buf-pos already accounts for hscroll
            [end-buf-pos (visual-line-end-buf-pos vl)])
       (if (>= target-col (gap-display-width gb buf-pos end-buf-pos))
           ;; Past end of this visual line → clamp to end of line (before newline if present)
           (if (and (> end-buf-pos buf-pos)
                    (= (gap-byte-ref gb (sub1 end-buf-pos)) #x0A))
               (sub1 end-buf-pos)
               end-buf-pos)
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

  ;; 1. Vertical: point above viewport → scroll up; below → scroll down.
  ;;    point-below? counts rows, so pt at buffer end on the last
  ;;    visible row is correctly treated as visible (the old
  ;;    `>= last-visible-pos` check forced a spurious vertical scroll
  ;;    there, discarding the horizontal scroll).
  (define new-start
    (cond [(< pt-pos start-pos)
           ;; Point is above — scroll up so point is on first line
           (let ([nl (gap-scan-byte gb pt-pos 'backward (λ (b) (= b #x0A)))])
             (if (>= nl 0) (add1 nl) 0))]
          [(point-below? gb pt-pos start-pos max-rows max-cols wrap-mode len)
           ;; Point is below — scroll down so point is 1/3 from bottom
           (define target-lines (max 1 (quotient (* max-rows 2) 3)))
           (beginning-of-nth-prev-line gb pt-pos target-lines)]
          [else
           ;; Point is visible — keep current scroll
           #f]))

  ;; 2. Horizontal (truncate mode only), relative to the post-vscroll
  ;;    baseline: after a vertical scroll hscroll resets to 0 so a
  ;;    long-line hscroll doesn't leak into shorter lines.
  (define base-h (if new-start 0 hscroll))
  (define new-hscroll
    (if (eq? wrap-mode 'none)
        (let* ([bol (gap-scan-byte gb pt-pos 'backward (λ (b) (= b #x0A)))]
               [line-start (if (>= bol 0) (add1 bol) 0)]
               [pt-col (gap-display-width gb line-start pt-pos)])
          (cond [(< pt-col base-h)
                 ;; Point left of visible area — pt-col is inherently a
                 ;; char boundary on point's line
                 pt-col]
                [(>= pt-col (+ base-h max-cols))
                 ;; Point right of visible area — snap the raw hscroll
                 ;; up to a char boundary of point's line, so screen
                 ;; col 0 aligns with a char start and the cursor never
                 ;; lands on the trailing cell of a wide char.
                 (hscroll-char-boundary gb line-start pt-pos
                                        (max 0 (- pt-col max-cols -1)))]
                [else base-h]))
        base-h))  ; wrap mode: keep baseline (no hscroll)

  (values (or new-start start-pos) new-hscroll))

;; ============================================================
;; Scroll helpers (internal)
;; ============================================================

(define (point-below? gb pt start max-rows max-cols wrap len)
  ;; Is PT below the viewport's last visible row?
  ;; pt on the last displayed row (including pt = buffer end) counts
  ;; as visible — the cursor may sit at end-of-buffer.
  (define (nl? b) (= b #x0A))
  (if (eq? wrap 'none)
      ;; Row of pt = number of newlines in [start, pt).  Below iff ≥ max-rows.
      (let loop ([pos start] [row 0])
        (cond [(>= row max-rows) #t]
              [else
               (define nl (gap-scan-byte gb pos 'forward nl?))
               (if (>= nl pt)
                   #f                       ; pt is on this row
                   (loop (add1 nl) (add1 row)))]))
      (let ([vlines (visual-line-lines gb start max-rows max-cols
                                       #:wrap-mode 'char)])
        (cond [(null? vlines) (> pt start)]
              [else
               (define last-end (visual-line-end-buf-pos (last vlines)))
               (or (> pt last-end)
                   (and (= pt last-end)
                        (or (< last-end len)
                            (= (length vlines) max-rows))))]))))

(define (hscroll-char-boundary gb line-start pt-pos raw)
  ;; Smallest display column ≥ RAW that falls on a char boundary of
  ;; point's line.  scan-display-width stops before the char that
  ;; would exceed RAW; if that leaves us short (a wide char straddles
  ;; RAW), advance past that char.
  (define seg (scan-display-width gb line-start pt-pos raw))
  (define w (gap-display-width gb line-start seg))
  (if (or (>= w raw) (>= seg pt-pos))
      w
      (+ w (max 0 (char-display-width (gap-char gb seg))))))

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
