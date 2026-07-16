#lang racket

;; display/render.rkt — layout + faces → vbuffer
;;
;; Pure fill: walks layout visual-lines, resolves face-ids per character
;; from text properties, fills a vbuffer.  No terminal output, no ANSI.
;;
;; Dependencies: kernel/data (gap+query), kernel/buffer (textprop),
;;   display/vbuffer, display/layout, display/face, display/char-width

(require "../kernel/data/gap.rkt"
         "../kernel/data/query.rkt"
         "../kernel/data/textprop.rkt"
         "vbuffer.rkt"
         "layout.rkt"
         "face.rkt"
         "char-width.rkt")

(provide
 render-layout!
 render-layout/region!)

;; ============================================================
;; render-layout! — fill vbuffer from a layout
;; ============================================================

(define (render-layout! ly gb text-props face-cache)
  ;; Returns a fresh vbuffer filled from the layout.
  ;; Face-ids are resolved from text-properties via face-cache.
  (define rows (layout-max-rows ly))
  (define cols (layout-max-cols ly))
  (define vb  (make-vbuffer rows cols))

  (for ([vl  (in-list (layout-lines ly))]
        [row (in-naturals)]
        #:when (< row rows))
    (render-visual-line! vb row 0 vl gb text-props face-cache #f #f))

  vb)

;; ============================================================
;; render-layout/region! — with region highlight overlay
;; ============================================================

(define (render-layout/region! ly gb text-props face-cache
                               region-beg region-end)
  (define rows (layout-max-rows ly))
  (define cols (layout-max-cols ly))
  (define vb  (make-vbuffer rows cols))

  (for ([vl  (in-list (layout-lines ly))]
        [row (in-naturals)]
        #:when (< row rows))
    (render-visual-line! vb row 0 vl gb text-props face-cache
                         region-beg region-end))

  vb)

;; ============================================================
;; render-visual-line! — one visual-line → one vbuffer row
;; ============================================================

(define (render-visual-line! vb row start-col vl gb text-props
                             face-cache region-beg region-end)
  (define content     (visual-line-content vl))
  (define buf-pos     (visual-line-buf-pos vl))
  (define truncated?  (visual-line-truncated? vl))
  (define max-cols    (vbuffer-cols vb))

  ;; Face-id for base text (from text-properties) + region overlay
  (define face-cache-map (make-hash))  ; (base-face . overlay) → face-id

  (let loop ([col      start-col]
             [char-idx 0]
             [byte-pos buf-pos])
    (when (< char-idx (string-length content))
      (define ch (string-ref content char-idx))

      ;; Resolve face-id for this position
      (define base-face (textprop-face-at text-props byte-pos))
      (define in-region? (and region-beg region-end
                              (>= byte-pos region-beg)
                              (<  byte-pos region-end)))
      (define overlay (and in-region? region-face))
      (define fid
        (if (or base-face overlay)
            (let ([key (cons base-face overlay)])
              (hash-ref! face-cache-map key
                (λ () (or (face-id-with-overlay base-face overlay face-cache) 0))))
            0))

      ;; Character display width
      (define cw (max 0 (char-display-width ch)))

      ;; Tab → spaces
      (cond [(char=? ch #\tab)
             (define tab-w (tab-width))
             (let fill-tab ([c col] [n tab-w])
               (when (and (< c max-cols) (> n 0))
                 (vbuffer-put-char! vb row c #\space #:face-id fid)
                 (fill-tab (add1 c) (sub1 n))))
             (loop (+ col tab-w)
                   (add1 char-idx)
                   (gap-next-char-pos gb byte-pos))]

            ;; Control characters → ^X representation
            [(< (char->integer ch) 32)
             (when (< col max-cols)
               (vbuffer-put-char! vb row col #\^ #:face-id fid))
             (when (< (add1 col) max-cols)
               (vbuffer-put-char! vb row (add1 col)
                                  (integer->char (+ 64 (char->integer ch)))
                                  #:face-id fid))
             (loop (+ col 2)
                   (add1 char-idx)
                   (gap-next-char-pos gb byte-pos))]

            ;; DEL character
            [(char=? ch #\rubout)
             (when (< col max-cols)
               (vbuffer-put-char! vb row col #\^ #:face-id fid))
             (when (< (add1 col) max-cols)
               (vbuffer-put-char! vb row (add1 col) #\? #:face-id fid))
             (loop (+ col 2)
                   (add1 char-idx)
                   (gap-next-char-pos gb byte-pos))]

            ;; Normal character
            [else
             (when (< col max-cols)
               (vbuffer-put-char! vb row col ch #:face-id fid))
             ;; Skip next column for wide chars (display-width=2)
             (loop (+ col (if (= cw 2) 2 1))
                   (add1 char-idx)
                   (gap-next-char-pos gb byte-pos))])))

  ;; Truncation marker
  (when (and truncated? (> max-cols 0))
    (vbuffer-put-char! vb row (sub1 max-cols) #\$))

  vb)

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit
           "../kernel/data/text.rkt"
           "../kernel/data/textprop.rkt"
           "../platform/ansi.rkt")

  (define (make-gb str) (text-gap (make-text str)))
  (define (make-tp) (make-text-properties))

  (parameterize ([color-depth 'truecolor])
    (init-face-cache!)
    (define-face! 'keyword (make-face-attrs attr-foreground 3))
    (define-face! 'comment (make-face-attrs attr-foreground 6 attr-weight 'bold))

    (test-case "render simple text"
      (let* ([gb (make-gb "hello")]
             [tp (make-tp)]
             [fc (current-face-cache)]
             [ly (compute-layout gb 0 #:max-rows 3 #:max-cols 10)]
             [vb (render-layout! ly gb tp fc)])
        (check-equal? (vbuffer-row->string vb 0) "hello     ")
        (check-equal? (cell-face-id (vector-ref (vbuffer-cells vb) 0)) 0)))

    (test-case "render with face from text-properties"
      (let* ([gb (make-gb "x comment")]
             [tp (make-tp)]
             [fc (current-face-cache)])
        (textprop-put! tp 2 9 'face 'comment)
        (define ly (compute-layout gb 0 #:max-rows 3 #:max-cols 15))
        (define vb (render-layout! ly gb tp fc))
        ;; 'x' (pos 0-1) should have face-id 0
        (check-equal? (cell-face-id (vector-ref (vbuffer-cells vb) 0)) 0)
        ;; 'c' (pos 2) should have non-zero face-id
        (check-true (> (cell-face-id (vector-ref (vbuffer-cells vb) 2)) 0))))

    (test-case "render with region overlay"
      (let* ([gb (make-gb "hello world")]
             [tp (make-tp)]
             [fc (current-face-cache)]
             [ly (compute-layout gb 0 #:max-rows 3 #:max-cols 15)]
             [vb (render-layout/region! ly gb tp fc 3 8)])
        ;; Before region (pos 0-2)
        (check-equal? (cell-face-id (vector-ref (vbuffer-cells vb) 0)) 0)
        ;; Inside region (pos 3-7)
        (check-true (> (cell-face-id (vector-ref (vbuffer-cells vb) 3)) 0))
        ;; After region (pos 8+)
        (check-equal? (cell-face-id (vector-ref (vbuffer-cells vb) 8)) 0)))

    (test-case "tab expansion"
      (let* ([gb (make-gb "\thello")]
             [tp (make-tp)]
             [fc (current-face-cache)]
             [ly (compute-layout gb 0 #:max-rows 3 #:max-cols 15)])
        (parameterize ([tab-width 4])
          (define vb (render-layout! ly gb tp fc))
          (check-equal? (cell-ch (vector-ref (vbuffer-cells vb) 0)) #\space)
          (check-equal? (cell-ch (vector-ref (vbuffer-cells vb) 4)) #\h))))

    (test-case "truncation marker"
      (let* ([gb (make-gb "this text is way too long")]
             [tp (make-tp)]
             [fc (current-face-cache)]
             [ly (compute-layout gb 0 #:max-rows 3 #:max-cols 10)])
        (define vb (render-layout! ly gb tp fc))
        ;; Last column should be '$'
        (check-equal? (cell-ch (vector-ref (vbuffer-cells vb) 9)) #\$))))
)
