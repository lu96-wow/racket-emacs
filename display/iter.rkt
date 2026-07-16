#lang racket

;; display/iter.rkt — Display Iterator (buffer → glyph state machine)
;;
;; The iterator walks a gap buffer producing display elements.
;; Separates "what glyph comes next" from "what to do with it".
;; Fields are mutable — the caller advances the iterator in place.
;;
;; Inspired by Emacs's `struct it`.

(require racket/function
         "../kernel/gap/gap.rkt"
         "../kernel/gap/query.rkt"
         "../kernel/vbuffer/vbuffer.rkt"
         "char-width.rkt")

(provide
 ;; struct
 display-iter? display-iter
 display-iter-buf display-iter-gb
 display-iter-pos display-iter-bol display-iter-face-id
 display-iter-row display-iter-col
 display-iter-max-cols display-iter-wrap-mode display-iter-left-col
 display-iter-buf-len
 set-display-iter-pos! set-display-iter-bol! set-display-iter-face-id!
 set-display-iter-row! set-display-iter-col!
 set-display-iter-max-cols! set-display-iter-wrap-mode! set-display-iter-left-col!

 ;; construct
 make-display-iter

 ;; walk (mutate it in place)
 di-next!     ; → (values char int int) | 'row-end | 'eob
 di-fill-row! ; → fill vbuffer row, returns #t or 'eob

 ;; position
 di-seek!       ; jump to buffer position
 di-eob?        ; at end of buffer?

 ;; query
 di-at-bol?
 di-col-from-bol
 )

;; ============================================================
;; Struct
;; ============================================================

(struct display-iter
  (buf       ; buffer?
   gb        ; gap-buffer?
   [pos #:mutable]       ; current byte position
   [bol #:mutable]       ; beginning of current logical line
   [face-id #:mutable]   ; current face (0 = default)
   [row #:mutable]       ; current visual row
   [col #:mutable]       ; current display column in this row
   [max-cols #:mutable]  ; window width
   [wrap-mode #:mutable] ; 'none or 'char
   [left-col #:mutable]  ; horizontal scroll offset
   buf-len)              ; cached buffer length
  #:transparent)

;; ============================================================
;; Constructor
;; ============================================================

(define (make-display-iter buf start-pos
                           #:max-cols [max-cols 80]
                           #:wrap-mode [wrap-mode 'none]
                           #:left-col [left-col 0]
                           #:face-id [face-id 0]
                           #:row [row 0]
                           #:col [col 0])
  (define gb (text-gap (buffer-text buf)))
  (define len (gap-length gb))
  (define pos (min start-pos len))
  (define bol (calc-bol gb pos))
  (display-iter buf gb pos bol face-id row col max-cols wrap-mode left-col len))

(require "../kernel/buffer.rkt"
         "../kernel/text.rkt")

(define (calc-bol gb pos)
  (let loop ([p pos])
    (if (<= p 0) 0
        (let ([pp (gap-prev-char-pos gb p)])
          (if (char=? (gap-char gb pp) #\newline)
              p  ; pp points to \n, p is past it
              (loop pp))))))

;; ============================================================
;; di-eob? / di-at-bol? / di-col-from-bol
;; ============================================================

(define (di-eob? it)
  (>= (display-iter-pos it) (display-iter-buf-len it)))

(define (di-at-bol? it)
  (= (display-iter-pos it) (display-iter-bol it)))

(define (di-col-from-bol it)
  (gap-display-width (display-iter-gb it)
                     (display-iter-bol it)
                     (display-iter-pos it)))

;; ============================================================
;; di-seek! — jump and recalculate bol
;; ============================================================

(define (di-seek! it new-pos)
  (define len (display-iter-buf-len it))
  (define pos (min new-pos len))
  (define gb (display-iter-gb it))
  (set-display-iter-pos! it pos)
  (set-display-iter-bol! it (calc-bol gb pos))
  (set-display-iter-col! it 0))

;; ============================================================
;; di-next! — advance one character
;; ============================================================
;; Returns:
;;   (values char cw face-id)  — consumed a character
;;   'row-end                   — end of visual row (newline or overflow)
;;   'eob                       — end of buffer (no more content)
;;
;; Handles: newline, truncation, wrapping, hscroll

(define (di-next! it)
  (define pos   (display-iter-pos it))
  (define len   (display-iter-buf-len it))
  (define col   (display-iter-col it))
  (define max-cols (display-iter-max-cols it))
  (define wrap  (display-iter-wrap-mode it))
  (define lcol  (display-iter-left-col it))
  (define gb    (display-iter-gb it))
  (define fid   (display-iter-face-id it))

  ;; ---- eob ----
  (when (>= pos len)
    'eob)

  ;; ---- character at position ----
  (define ch (gap-char gb pos))
  (define cw (max 1 (char-display-width ch)))

  ;; ---- newline: row-end immediately ----
  (when (char=? ch #\newline)
    (define next-pos (gap-next-char-pos gb pos))
    (set-display-iter-pos! it next-pos)
    (set-display-iter-bol! it next-pos)
    (set-display-iter-col! it 0)
    (set-display-iter-row! it (add1 (display-iter-row it)))
    'row-end)

  ;; ---- hscroll: skip characters before left-col ----
  (when (> lcol 0)
    (define col-from-bol (gap-display-width gb (display-iter-bol it) pos))
    (when (< col-from-bol lcol)
      (define next-pos (gap-next-char-pos gb pos))
      (set-display-iter-pos! it next-pos)
      ;; col stays 0 since we're in scrolled-out region
      (di-next! it)))

  ;; ---- truncate mode: check overflow ----
  (when (eq? wrap 'none)
    (define eff-col (+ col lcol))
    (when (>= eff-col max-cols)
      ;; Exceeded width — fast-forward to end of this logical line
      (skip-to-eol! it)
      'row-end))

  ;; ---- wrap mode: check wrap point ----
  (when (and (eq? wrap 'char) (> col 0) (> (+ col cw) max-cols))
    ;; Don't consume this character, row ends here
    'row-end)

  ;; ---- Normal: consume the character ----
  (define next-pos (gap-next-char-pos gb pos))
  (set-display-iter-pos! it next-pos)
  (set-display-iter-col! it (+ col cw))
  (values ch cw fid))

;; Helper: skip to end of current logical line
(define (skip-to-eol! it)
  (define gb (display-iter-gb it))
  (define len (display-iter-buf-len it))
  (define pos (display-iter-pos it))
  (define nl (gap-scan-byte gb pos 'forward (curry = #x0A)))
  (if (< nl len)
      (let ([next-pos (gap-next-char-pos gb nl)])
        (set-display-iter-pos! it next-pos)
        (set-display-iter-bol! it next-pos)
        (set-display-iter-col! it 0)
        (set-display-iter-row! it (add1 (display-iter-row it))))
      (set-display-iter-pos! it len)))

;; ============================================================
;; di-fill-row! — fill one vbuffer row from iterator
;; ============================================================
;; Returns:
;;   #t if another row follows
;;   #f if this is the last row
;;   'eob if buffer ended

(define (di-fill-row! vb row-num it)
  (define max-cols (display-iter-max-cols it))
  (define glyphs '())
  (define row-start (display-iter-pos it))

  (let loop ()
    (define r (di-next! it))
    (cond
      [(eq? r 'eob)
       (if (null? glyphs)
           'eob
           (begin (write-glyphs! vb row-num (reverse glyphs) max-cols)
                  #f))]  ; last row
      [(eq? r 'row-end)
       (write-glyphs! vb row-num (reverse glyphs) max-cols)
       #t]  ; more rows follow
      [else
       (let-values ([(ch cw fid) r])
         (set! glyphs (cons (list ch cw fid) glyphs))
         (loop))])))

;; Write glyphs into vbuffer at given row
(define (write-glyphs! vb row-num glyphs max-cols)
  (for ([g (in-list glyphs)]
        [col (in-naturals)]
        #:when (< col max-cols))
    (match-define (list ch cw fid) g)
    (vbuffer-put-char! vb row-num col ch #:face-id fid)))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit
           "../kernel/buffer.rkt"
           "../kernel/text.rkt")

  (define (mk-it text #:max-cols [mc 10] #:wrap [w 'none])
    (define b (make-buffer "test" text))
    (make-display-iter b 0 #:max-cols mc #:wrap-mode w))

  ;; Basic walk
  (let ([it (mk-it "hello")])
    (let-values ([(ch cw fid) (di-next! it)])
      (check-equal? ch #\h)
      (check-equal? cw 1))
    (check-equal? (display-iter-pos it) 1))

  ;; Newline → row-end
  (let ([it (mk-it "a\nb")])
    (di-next! it)
    (check-equal? (di-next! it) 'row-end)
    (check-equal? (display-iter-bol it) 2)
    (let-values ([(ch cw fid) (di-next! it)])
      (check-equal? ch #\b)))

  ;; EOB
  (let ([it (mk-it "x")])
    (di-next! it)
    (check-equal? (di-next! it) 'eob))

  ;; Truncation: "abcdefghijklmnop" with max-cols=5
  (let ([it (mk-it "abcdefghijklmnop" #:max-cols 5)])
    (for ([i (in-range 5)]) (di-next! it)) ; a,b,c,d,e
    (check-equal? (di-next! it) 'row-end)   ; truncated
    (check-equal? (display-iter-bol it) 16)) ; past \n (which doesn't exist, so eob)

  ;; Wrap: "abcdefghij" with max-cols=4, wrap=char
  (let ([it (mk-it "abcdefghij" #:max-cols 4 #:wrap 'char)])
    (for ([i (in-range 4)]) (di-next! it))
    (check-equal? (di-next! it) 'row-end)  ; wraps
    (check-equal? (display-iter-pos it) 4)) ; hasn't consumed 5th char
)
