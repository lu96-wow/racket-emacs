#lang racket

;; base/edit.rkt — Editing commands (composed from kernel primitives)
;;
;; forward-char = (buffer-point) + (set-buffer-point!) on gap-char-at result.
;; All operations are pure compositions — no display or IO.

(require "../kernel/buffer.rkt"
         "../kernel/gap.rkt"
         "../kernel/marker.rkt"
         "../kernel/char-width.rkt"
         "../kernel/kill-ring.rkt"
         "../kernel/syntax.rkt")

(provide
 ;; ── movement ──
 forward-char backward-char
 beginning-of-line end-of-line
 forward-line backward-line
 beginning-of-buffer end-of-buffer
 forward-word backward-word

 ;; ── shift-select ──
 shift-forward-char shift-backward-char
 shift-forward-line shift-backward-line
 shift-forward-word shift-backward-word

 ;; ── editing ──
 insert delete-char delete-backward-char
 delete-region kill-line newline
 undo redo
 yank yank-pop

 ;; ── symbol / thing-at-point ──
 symbol-at-point
 lisp-identifier-char?

 ;; ── mark / region ──
 exchange-point-and-mark
 mark-whole-buffer kill-region
 region-string deactivate-mark

 ;; ── point helpers ──
 point point-at-bol point-at-eol
 current-column move-to-column

 ;; ── display toggles ──
 toggle-truncate-lines scroll-left scroll-right

 ;; ── excursion ──
 save-excursion with-current-buffer

 ;; ── command protocol ──
 last-command this-command)

;; ============================================================
;; Helpers
;; ============================================================

(define (newline-byte? b) (= b #x0A))
(define (buf-gb [b #f]) (buffer-gap (or b (current-buffer))))

(define (forward-char-bytes gb byte-pos n)
  (let loop ([i n] [pos byte-pos])
    (if (or (zero? i) (>= pos (gap-byte-length gb))) pos
        (let-values ([(ch clen) (gap-char-at gb pos)]) (loop (sub1 i) (+ pos clen))))))

(define (backward-char-bytes gb byte-pos n)
  (let loop ([i n] [pos byte-pos])
    (if (or (zero? i) (<= pos 0)) (max 0 pos) (loop (sub1 i) (gap-prev-char-pos gb pos)))))

;; ============================================================
;; Command protocol
;; ============================================================

(define last-command (make-parameter #f))
(define this-command (make-parameter #f))

;; ============================================================
;; Movement
;; ============================================================

(define (forward-char [n 1] #:buf [b #f])
  (define buf (or b (current-buffer))) (define gb (buffer-gap buf))
  (set-buffer-point! buf (forward-char-bytes gb (buffer-point buf) n)))

(define (backward-char [n 1] #:buf [b #f])
  (define buf (or b (current-buffer))) (define gb (buffer-gap buf))
  (set-buffer-point! buf (backward-char-bytes gb (buffer-point buf) n)))

(define (beginning-of-buffer #:buf [b #f])
  (define buf (or b (current-buffer))) (set-buffer-point! buf (buffer-begv buf)))

(define (end-of-buffer #:buf [b #f])
  (define buf (or b (current-buffer))) (set-buffer-point! buf (buffer-zv buf)))

(define (beginning-of-line #:buf [b #f])
  (define buf (or b (current-buffer))) (define gb (buffer-gap buf))
  (define p (buffer-point buf)) (define nl (gap-scan-backward-byte gb p newline-byte?))
  (set-buffer-point! buf (if (>= nl 0) (add1 nl) (buffer-begv buf))))

(define (end-of-line #:buf [b #f])
  (define buf (or b (current-buffer))) (define gb (buffer-gap buf))
  (define p (buffer-point buf)) (define len (gap-byte-length gb))
  (define nl (gap-scan-forward-byte gb p newline-byte?))
  (set-buffer-point! buf (if (< nl len) nl len)))

(define goal-column (box #f))

(define (update-goal-column! buf)
  (define prev (last-command))
  (unless (or (eq? prev forward-line) (eq? prev backward-line))
    (set-box! goal-column (current-column #:buf buf))))

(define (move-line forward? n buf)
  (define gb (buffer-gap buf)) (define len (gap-byte-length gb))
  (update-goal-column! buf)
  (define cur-bol (let ([nl (gap-scan-backward-byte gb (buffer-point buf) newline-byte?)])
                    (if (>= nl 0) (add1 nl) 0)))
  (define target-bol
    (let loop ([pos cur-bol] [rem n])
      (cond [(<= rem 0) (if forward? pos
                           (let ([nl (gap-scan-backward-byte gb pos newline-byte?)])
                             (if (>= nl 0) (add1 nl) 0)))]
            [forward? (if (>= pos len) len
                         (let ([nl (gap-scan-forward-byte gb pos newline-byte?)])
                           (if (>= nl len) len (loop (add1 nl) (sub1 rem)))))]
            [(<= pos 0) 0]
            [else (let ([nl (gap-scan-backward-byte gb pos newline-byte?)])
                    (if (< nl 0) 0 (loop nl (sub1 rem))))])))
  (set-buffer-point! buf target-bol)
  (define col (unbox goal-column)) (when col (move-to-column col #:buf buf)))

(define (forward-line [n 1] #:buf [b #f])
  (define buf (or b (current-buffer))) (move-line #t n buf) (this-command forward-line))

(define (backward-line [n 1] #:buf [b #f])
  (define buf (or b (current-buffer))) (move-line #f n buf) (this-command backward-line))

(define (point #:buf [b #f]) (buffer-point (or b (current-buffer))))
(define (point-at-bol #:buf [b #f])
  (define buf (or b (current-buffer))) (define gb (buffer-gap buf))
  (define nl (gap-scan-backward-byte gb (buffer-point buf) newline-byte?))
  (if (>= nl 0) (add1 nl) 0))
(define (point-at-eol #:buf [b #f])
  (define buf (or b (current-buffer))) (define gb (buffer-gap buf))
  (define len (gap-byte-length gb)) (define nl (gap-scan-forward-byte gb (buffer-point buf) newline-byte?))
  (if (< nl len) nl len))

;; ============================================================
;; Word movement
;; ============================================================

(define (cjk-char? ch)
  (define cp (char->integer ch))
  (or (<= #x2E80 cp #x9FFF) (<= #xF900 cp #xFAFF) (<= #xFE30 cp #xFE4F)
      (<= #xFF01 cp #xFF60) (<= #x20000 cp #x2FFFF) (<= #x30000 cp #x3FFFF)))

(define (forward-word [n 1] #:buf [b #f])
  (define buf (or b (current-buffer))) (define st (buffer-syntax-table buf))
  (define gb (buffer-gap buf)) (define len (gap-byte-length gb))
  (let loop ([p (buffer-point buf)] [count n])
    (cond [(>= p len) (set-buffer-point! buf len)] [(<= count 0) (set-buffer-point! buf p)]
          [else (define-values (ch0 clen0) (gap-char-at gb p))
           (if (and (char-word? ch0 st) (cjk-char? ch0)) (loop (+ p clen0) (sub1 count))
               (let* ([p1 (gap-scan-forward-char gb p (λ (ch) (not (char-word? ch st))))]
                      [p2 (gap-scan-forward-char gb (min p1 len) (λ (ch) (char-word? ch st)))]
                      [p3 (gap-scan-forward-char gb (min p2 len) (λ (ch) (not (char-word? ch st))))])
                 (loop (min p3 len) (sub1 count))))])))

(define (backward-word [n 1] #:buf [b #f])
  (define buf (or b (current-buffer))) (define st (buffer-syntax-table buf))
  (define gb (buffer-gap buf))
  (let loop ([p (buffer-point buf)] [count n])
    (cond [(<= p 0) (set-buffer-point! buf 0)] [(<= count 0) (set-buffer-point! buf p)]
          [else
           ;; Skip backward over non-word chars first (like Emacs)
           (define p1 (gap-scan-backward-char gb p (λ (ch) (char-word? ch st))))
           (cond
             [(< p1 0) (loop 0 (sub1 count))]
             [(cjk-char? (let-values ([(ch cl) (gap-char-at gb p1)]) ch))
              ;; CJK: single-char word
              (loop p1 (sub1 count))]
             [else
              ;; Skip backward over word chars to find start of word
              (define p2 (gap-scan-backward-char gb (+ p1 1) (λ (ch) (not (char-word? ch st)))))
              (if (< p2 0)
                  (loop 0 (sub1 count))
                  (let-values ([(ch2 c2) (gap-char-at gb p2)])
                    (loop (+ p2 c2) (sub1 count))))])])))

;; ============================================================
;; symbol-at-point — extract the identifier under cursor
;; ============================================================

;; Characters that are valid inside Racket/Lisp identifiers
;; but not classified as 'word or 'symbol in the syntax table.
;; Racket §1.1: identifiers may contain special chars like -!$%^&*+=~/<>?.
(define lisp-ident-extra-chars
  (list->set (map (λ (s) (string-ref s 0))
                  (list "-" "!" "$" "%" "^" "&" "*"
                        "+" "=" "~" "/" "?" "<" ">" "."))))

(define (lisp-identifier-char? ch st)
  ;; A character is part of a Lisp identifier if:
  ;; - it's 'word or 'symbol per syntax-table, OR
  ;; - it's one of the extra punctuation chars Racket allows in identifiers
  (or (char-word? ch st)
      (char-symbol? ch st)
      (set-member? lisp-ident-extra-chars ch)))

(define (symbol-at-point #:buf [b #f])
  ;; Return the identifier string at the current point position.
  ;; Uses syntax-table character classes + Racket extended identifier chars.
  ;; Handles |...| delimited identifiers.
  ;; Returns #f when point is not on an identifier character.
  (define buf (or b (current-buffer)))
  (define st (buffer-syntax-table buf))
  (define gb (buffer-gap buf))
  (define len (gap-byte-length gb))
  (define pt (buffer-point buf))
  ;; Bounds check
  (when (>= pt len) (set! pt (max 0 (sub1 len))))
  (define-values (ch cl) (gap-char-at gb pt))

  (let/ec return
    ;; ── Handle |...| delimited identifiers ──
    (when (char=? ch #\|)
      (define end-pipe (gap-scan-forward-byte gb (+ pt cl) (curry char=? #\|)))
      (if (< end-pipe len)
          (return (buffer-substring buf pt (add1 end-pipe)))
          (return #f)))

    ;; Check if point is on an identifier char
    (define sym-pos
      (cond [(lisp-identifier-char? ch st) pt]
            [(> pt 0)
             (define prev (gap-prev-char-pos gb pt))
             (define-values (pch pcl) (gap-char-at gb prev))
             (if (lisp-identifier-char? pch st) prev #f)]
            [else #f]))
    (unless sym-pos (return #f))

    ;; ── Scan backward to find start ──
    (define start
      (let loop ([p sym-pos])
        (if (<= p 0)
            0
            (let ([prev (gap-prev-char-pos gb p)])
              (if prev
                  (let-values ([(pch pcl) (gap-char-at gb prev)])
                    (if (lisp-identifier-char? pch st)
                        (loop prev)
                        p))
                  p)))))

    ;; ── Scan forward to find end ──
    (define end
      (let loop ([p sym-pos])
        (if (>= p len)
            len
            (let-values ([(ech ecl) (gap-char-at gb p)])
              (if (lisp-identifier-char? ech st)
                  (loop (+ p ecl))
                  p)))))

    (if (< start end)
        (buffer-substring buf start end)
        #f)))

;; ============================================================
;; Shift-select
;; ============================================================

(define (shift-select-move! mover [n 1])
  (define buf (current-buffer))
  (unless (region-active? #:buf buf) (set-mark #:buf buf)) (mover n #:buf buf))

(define (shift-forward-char [n 1])     (shift-select-move! forward-char n))
(define (shift-backward-char [n 1])    (shift-select-move! backward-char n))
(define (shift-forward-line [n 1])     (shift-select-move! forward-line n))
(define (shift-backward-line [n 1])    (shift-select-move! backward-line n))
(define (shift-forward-word [n 1])     (shift-select-move! forward-word n))
(define (shift-backward-word [n 1])    (shift-select-move! backward-word n))

;; ============================================================
;; Editing
;; ============================================================

(define (insert str #:buf [b #f])
  (define buf (or b (current-buffer)))
  (when (region-active? #:buf buf)
    (define beg (region-beginning #:buf buf)) (define end (region-end #:buf buf))
    (buffer-delete buf beg end) (set-buffer-mark! buf #f))
  (buffer-insert buf str))

(define (delete-char [n 1] #:buf [b #f])
  (define buf (or b (current-buffer)))
  (if (region-active? #:buf buf)
      (begin (delete-region #:buf buf) (set-buffer-mark! buf #f))
      (let* ([gb (buffer-gap buf)] [p (buffer-point buf)]
             [end (min (forward-char-bytes gb p n) (buffer-zv buf))])
        (when (> end p) (buffer-delete buf p end)))))

(define (delete-backward-char [n 1] #:buf [b #f])
  (define buf (or b (current-buffer)))
  (if (region-active? #:buf buf)
      (begin (delete-region #:buf buf) (set-buffer-mark! buf #f))
      (let* ([gb (buffer-gap buf)] [p (buffer-point buf)]
             [start (max (backward-char-bytes gb p n) (buffer-begv buf))])
        (when (< start p) (buffer-delete buf start p)))))

(define (delete-region #:buf [b #f])
  (define buf (or b (current-buffer)))
  (when (region-active? #:buf buf)
    (define beg (region-beginning #:buf buf)) (define end (region-end #:buf buf))
    (buffer-delete buf beg end) (set-buffer-mark! buf #f)))

(define (deactivate-mark #:buf [b #f]) (set-buffer-mark! (or b (current-buffer)) #f))

(define (region-string #:buf [b #f])
  (define buf (or b (current-buffer)))
  (if (region-active? #:buf buf)
      (buffer-substring buf (region-beginning #:buf buf) (region-end #:buf buf))
      ""))

(define (kill-line #:buf [b #f])
  (define buf (or b (current-buffer))) (define gb (buffer-gap buf))
  (define p (buffer-point buf)) (define len (gap-byte-length gb))
  (define nl (gap-scan-forward-byte gb p newline-byte?)) (define eol (if (< nl len) nl len))
  (define kill-from p)
  (define kill-to (cond [(< p eol) (if (< eol len) (add1 eol) len)]
                        [(and (< p len) (= (gap-byte-ref gb p) #x0A)) (add1 p)] [else #f]))
  (when kill-to
    (define killed-text (buffer-substring buf kill-from kill-to))
    (buffer-delete buf kill-from kill-to) (define prev (last-command))
    (if (or (eq? prev kill-line) (eq? prev kill-region)) (kill-append killed-text #f) (kill-new killed-text))
    (this-command kill-line)))

(define (newline #:buf [b #f])
  (define buf (or b (current-buffer)))
  (when (region-active? #:buf buf)
    (define beg (region-beginning #:buf buf)) (define end (region-end #:buf buf))
    (buffer-delete buf beg end) (set-buffer-mark! buf #f))
  (buffer-insert buf "\n"))

(define (undo #:buf [b #f]) (buffer-undo (or b (current-buffer))))
(define (redo #:buf [b #f]) (buffer-redo (or b (current-buffer))))

;; ============================================================
;; Mark / Region
;; ============================================================

(define (mark-whole-buffer #:buf [b #f])
  (define buf (or b (current-buffer))) (beginning-of-buffer #:buf buf)
  (define m (make-marker (buffer-zv buf) #f)) (set-buffer-mark! buf m)
  (set-buffer-markers! buf (cons m (buffer-markers buf))))

(define (exchange-point-and-mark #:buf [b #f])
  (define buf (or b (current-buffer))) (define m (buffer-mark buf))
  (unless m (set-mark #:buf buf) (set! m (buffer-mark buf)))
  (when m (define old-mark-pos (marker-pos m)) (define old-point (buffer-point buf))
    (set-buffer-point! buf old-mark-pos) (set-marker-pos! m old-point)
    (set-buffer-mark! buf m)))

(define (kill-region #:buf [b #f])
  (define buf (or b (current-buffer)))
  (when (region-active? #:buf buf)
    (define beg (region-beginning #:buf buf)) (define end (region-end #:buf buf))
    (define killed-text (buffer-substring buf beg end)) (buffer-delete buf beg end)
    (define prev (last-command))
    (if (or (eq? prev kill-region) (eq? prev kill-line)) (kill-append killed-text (< end beg)) (kill-new killed-text))
    (set-buffer-mark! buf #f) (this-command kill-region)))

;; ============================================================
;; Column
;; ============================================================

(define (current-column #:buf [b #f])
  (define buf (or b (current-buffer))) (define gb (buffer-gap buf)) (define pt (buffer-point buf))
  (define bol (let ([nl (gap-scan-backward-byte gb pt newline-byte?)]) (if (>= nl 0) (add1 nl) 0)))
  (gap-display-width gb bol pt))

(define (move-to-column col #:buf [b #f])
  (define buf (or b (current-buffer))) (define gb (buffer-gap buf)) (define len (gap-byte-length gb))
  (define bol (let ([nl (gap-scan-backward-byte gb (buffer-point buf) newline-byte?)]) (if (>= nl 0) (add1 nl) 0)))
  (define eol (let ([nl (gap-scan-forward-byte gb bol newline-byte?)]) (if (< nl len) nl len)))
  (set-buffer-point! buf (scan-display-width gb bol eol col)))

;; ============================================================
;; Display toggles
;; ============================================================

(define (scroll-left [n 1]) (void))
(define (scroll-right [n 1]) (void))
(define (toggle-truncate-lines) (define buf (current-buffer)) (set-truncate-lines?! (not (truncate-lines? buf)) buf))

;; ============================================================
;; Excursion
;; ============================================================

(define-syntax-rule (save-excursion body ...)
  (let* ([buf (current-buffer)] [pt-marker (make-marker (buffer-point buf) #f buf)])
    (dynamic-wind (λ () (set-buffer-markers! buf (cons pt-marker (buffer-markers buf))))
                  (λ () body ...)
                  (λ () (set-buffer-markers! buf (remove pt-marker (buffer-markers buf)))
                    (define mb (marker-buffer pt-marker))
                    (when mb (set-buffer mb) (set-buffer-point! mb (marker-pos pt-marker)))))))

(define-syntax-rule (with-current-buffer buf-expr body ...)
  (parameterize ([current-buffer (get-buffer buf-expr)]) body ...))

;; ============================================================
;; Yank
;; ============================================================

(define (yank #:buf [b #f])
  (define buf (or b (current-buffer)))
  (when (region-active? #:buf buf)
    (define beg (region-beginning #:buf buf)) (define end (region-end #:buf buf))
    (buffer-delete buf beg end) (set-buffer-mark! buf #f))
  (define text (kill-ring-yank)) (when text (buffer-insert buf text)))

(define (yank-pop #:buf [b #f])
  (define buf (or b (current-buffer))) (define prev (last-command))
  (when (eq? prev yank) (buffer-undo buf) (define text (kill-ring-pop)) (when text (buffer-insert buf text))))
