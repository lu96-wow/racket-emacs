#lang racket

;; display/render.rkt — layout + faces → vbuffer
;;
;; Pure fill: walks layout visual-lines, resolves face-ids per character
;; from text properties, fills a vbuffer.  No terminal output, no ANSI.
;;
;; Dependencies: kernel/data (gap+query), kernel/buffer (textprop),
;;   display/vbuffer, display/layout, display/face, kernel/data/char-width

(require "../kernel/data/gap.rkt"
         "../kernel/data/query.rkt"
         "../kernel/data/textprop.rkt"
         "vbuffer.rkt"
         "layout.rkt"
         "face.rkt"
         "../kernel/data/char-width.rkt"
         "row-cache.rkt")

(provide
 ;; without cache (always full render)
 render-layout!
 render-layout/region!

 ;; with row-cache (incremental where possible)
 render-layout/cached!
 render-layout/region/cached!)

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
    (define-values (_vb _glyphs)
      (render-visual-line! vb row 0 vl gb text-props face-cache #f #f))
    (void))

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
    (define-values (_vb _glyphs)
      (render-visual-line! vb row 0 vl gb text-props face-cache
                           region-beg region-end))
    (void))

  vb)

;; ============================================================
;; render-layout/cached! — incremental render with row-cache
;; ============================================================

(define (render-layout/cached! ly gb text-props face-cache row-cache)
  ;; Like render-layout! but uses row-cache for incremental redisplay.
  ;; On cache hit ('exact), skips face resolution and blits directly.
  ;; Returns vb; row-cache is mutated in-place.
  (define rows (layout-max-rows ly))
  (define cols (layout-max-cols ly))
  (define vb  (make-vbuffer rows cols))

  (define vlines (layout-lines ly))
  (for ([vl  (in-list vlines)]
        [row (in-naturals)]
        #:when (< row rows))
    (define buf-start (visual-line-buf-pos vl))
    (define buf-end   (visual-line-end-buf-pos vl))
    (match (row-cache-compare row-cache row buf-start buf-end)
      ['exact
       (row-cache-blit-row! vb row row-cache row)]
      [_  ;; 'shifted or 'stale — full render + update cache
       (define-values (_vb _glyphs) (render-visual-line! vb row 0 vl gb text-props
                                                          face-cache #f #f))
       (row-cache-update! row-cache row buf-start buf-end _glyphs
                          (visual-line-continued? vl)
                          (visual-line-truncated? vl))]))

  ;; Clear stale cache rows beyond current layout
  (row-cache-clear-from! row-cache (length vlines))
  vb)

;; ============================================================
;; render-layout/region/cached! — with region + row-cache
;; ============================================================

(define (render-layout/region/cached! ly gb text-props face-cache
                                       region-beg region-end row-cache)
  (define rows (layout-max-rows ly))
  (define cols (layout-max-cols ly))
  (define vb  (make-vbuffer rows cols))

  (define vlines (layout-lines ly))
  (for ([vl  (in-list vlines)]
        [row (in-naturals)]
        #:when (< row rows))
    (define buf-start (visual-line-buf-pos vl))
    (define buf-end   (visual-line-end-buf-pos vl))
    (match (row-cache-compare row-cache row buf-start buf-end)
      ['exact
       (row-cache-blit-row! vb row row-cache row)]
      [_
       (define-values (_vb _glyphs) (render-visual-line! vb row 0 vl gb text-props
                                                          face-cache
                                                          region-beg region-end))
       (row-cache-update! row-cache row buf-start buf-end _glyphs
                          (visual-line-continued? vl)
                          (visual-line-truncated? vl))]))

  (row-cache-clear-from! row-cache (length vlines))
  vb)

;; ============================================================
;; render-visual-line! — one visual-line → one vbuffer row
;; ============================================================

(define (render-visual-line! vb row start-col vl gb text-props
                             face-cache region-beg region-end)
  ;; Fill one vbuffer row from a visual-line.
  ;; Returns (values vb glyphs) where glyphs is (vectorof glyph?) for caching.
  (define content     (visual-line-content vl))
  (define buf-pos     (visual-line-buf-pos vl))
  (define truncated?  (visual-line-truncated? vl))
  (define max-cols    (vbuffer-cols vb))

  ;; Face-id for base text (from text-properties) + region overlay
  (define face-cache-map (make-hash))  ; (base-face . overlay) → face-id

  ;; Collect glyphs for row-cache (reversed, then reversed at end)
  (define glyphs-rev '())

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
                 (set! glyphs-rev (cons (glyph #\space 1 fid) glyphs-rev))
                 (fill-tab (add1 c) (sub1 n))))
             (loop (+ col tab-w)
                   (add1 char-idx)
                   (gap-next-char-pos gb byte-pos))]

            ;; Control characters → ^X representation
            [(< (char->integer ch) 32)
             (when (< col max-cols)
               (vbuffer-put-char! vb row col #\^ #:face-id fid)
               (set! glyphs-rev (cons (glyph #\^ 1 fid) glyphs-rev)))
             (when (< (add1 col) max-cols)
               (vbuffer-put-char! vb row (add1 col)
                                  (integer->char (+ 64 (char->integer ch)))
                                  #:face-id fid)
               (set! glyphs-rev (cons (glyph (integer->char (+ 64 (char->integer ch)))
                                             1 fid) glyphs-rev)))
             (loop (+ col 2)
                   (add1 char-idx)
                   (gap-next-char-pos gb byte-pos))]

            ;; DEL character
            [(char=? ch #\rubout)
             (when (< col max-cols)
               (vbuffer-put-char! vb row col #\^ #:face-id fid)
               (set! glyphs-rev (cons (glyph #\^ 1 fid) glyphs-rev)))
             (when (< (add1 col) max-cols)
               (vbuffer-put-char! vb row (add1 col) #\? #:face-id fid)
               (set! glyphs-rev (cons (glyph #\? 1 fid) glyphs-rev)))
             (loop (+ col 2)
                   (add1 char-idx)
                   (gap-next-char-pos gb byte-pos))]

            ;; Normal character
            [else
             (when (< col max-cols)
               (vbuffer-put-char! vb row col ch #:face-id fid))
             (set! glyphs-rev (cons (glyph ch cw fid) glyphs-rev))
             ;; Skip next column for wide chars (display-width=2)
             (loop (+ col (if (= cw 2) 2 1))
                   (add1 char-idx)
                   (gap-next-char-pos gb byte-pos))])))

  ;; Truncation marker
  (when (and truncated? (> max-cols 0))
    (vbuffer-put-char! vb row (sub1 max-cols) #\$))

  (values vb (list->vector (reverse glyphs-rev))))
