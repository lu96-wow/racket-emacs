#lang racket

;; display/render.rkt — layout + gap face-ids → vbuffer
;;
;; ============================================================================
;; Pure computation: walks layout visual-lines, reads face-ids directly
;; from the gap buffer (colocated with text), fills a vbuffer.
;; No terminal output, no ANSI, no text-properties involved.
;;
;; ============================================================================
;; Computation vs Application
;; ============================================================================
;;
;;   ── Pure (compute vbuffer from layout + gap) ──
;;     render-layout!  render-layout/region!
;;     render-layout/cached!  render-layout/region/cached!
;;
;;   The render is PURE — it reads from immutable layout and gap-buffer
;;   (face-ids are read-only during render).  The only "mutation" is
;;   writing to the freshly-allocated vbuffer, which is isolated.
;;
;; ============================================================================
;; Face-Id Resolution
;; ============================================================================
;;
;;   Face-ids are stored in the gap buffer directly (kernel/data/face.rkt).
;;   face-id 0 = default (no highlighting).
;;   face-id 1..N = registered faces (font-lock, bracket depths, etc.).
;;
;;   For region highlighting: the base face-id from the gap is merged with
;;   the region overlay face-id via face-cache.  Merged face-ids are cached
;;   per (base-fid . overlay-fid) pair for performance.
;;
;; ============================================================================
;; Dependencies
;; ============================================================================
;;
;;   kernel/data/gap.rkt        — gap-buffer? and gap accessors
;;   kernel/data/query.rkt      — gap-char, gap-next-char-pos
;;   kernel/data/face.rkt       — face-ref (O(1) face-id lookup)
;;   kernel/data/char-width.rkt — char-display-width
;;   display/vbuffer.rkt        — vbuffer cell grid
;;   display/layout.rkt         — layout, visual-line
;;   display/face.rkt           — face-cache, face-id-with-overlay-id
;;   display/row-cache.rkt      — incremental redisplay caching
;; ============================================================================

(require "../kernel/data/gap.rkt"
         "../kernel/data/query.rkt"
         "../kernel/data/face.rkt"
         "vbuffer.rkt"
         "layout.rkt"
         "face.rkt"
         "../kernel/data/char-width.rkt"
         "row-cache.rkt")

(provide
 ;; ── without cache (always full render) ──
 render-layout!
 render-layout/region!

 ;; ── with row-cache (incremental where possible) ──
 render-layout/cached!
 render-layout/region/cached!)

;; ============================================================
;; render-layout! — fill vbuffer from a layout (full render)
;; ============================================================

(define (render-layout! ly gb face-cache)
  ;; Returns a fresh vbuffer.  Face-ids are read directly from gap-buffer.
  ;; No text-properties involved.
  (unless (layout? ly)
    (raise-argument-error 'render-layout! "layout?" ly))
  (unless (gap-buffer? gb)
    (raise-argument-error 'render-layout! "gap-buffer?" gb))
  (define rows (layout-max-rows ly))
  (define cols (layout-max-cols ly))
  (define vb  (make-vbuffer rows cols))

  (for ([vl  (in-list (layout-lines ly))]
        [row (in-naturals)]
        #:when (< row rows))
    (define-values (_vb _glyphs)
      (render-visual-line! vb row 0 vl gb face-cache #f #f))
    (void))
  vb)

;; ============================================================
;; render-layout/region! — with region highlight overlay
;; ============================================================

(define (render-layout/region! ly gb face-cache region-beg region-end)
  ;; Like render-layout! but with active region highlighting.
  (unless (layout? ly)
    (raise-argument-error 'render-layout/region! "layout?" ly))
  (define rows (layout-max-rows ly))
  (define cols (layout-max-cols ly))
  (define vb  (make-vbuffer rows cols))

  (for ([vl  (in-list (layout-lines ly))]
        [row (in-naturals)]
        #:when (< row rows))
    (define-values (_vb _glyphs)
      (render-visual-line! vb row 0 vl gb face-cache
                           region-beg region-end))
    (void))
  vb)

;; ============================================================
;; render-layout/cached! — incremental render with row-cache
;; ============================================================

(define (render-layout/cached! ly gb face-cache row-cache)
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
       (define-values (_vb _glyphs)
         (render-visual-line! vb row 0 vl gb face-cache #f #f))
       (row-cache-update! row-cache row buf-start buf-end _glyphs
                          (visual-line-continued? vl)
                          (visual-line-truncated? vl))]))

  ;; Clear stale cache rows beyond current layout
  (row-cache-clear-from! row-cache (length vlines))
  vb)

;; ============================================================
;; render-layout/region/cached! — with region + row-cache
;; ============================================================

(define (render-layout/region/cached! ly gb face-cache
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
       (define-values (_vb _glyphs)
         (render-visual-line! vb row 0 vl gb face-cache
                              region-beg region-end))
       (row-cache-update! row-cache row buf-start buf-end _glyphs
                          (visual-line-continued? vl)
                          (visual-line-truncated? vl))]))

  (row-cache-clear-from! row-cache (length vlines))
  vb)

;; ============================================================
;; render-visual-line! — one visual-line → one vbuffer row
;; ============================================================

(define (render-visual-line! vb row start-col vl gb
                             face-cache region-beg region-end)
  ;; Fill one vbuffer row from a visual-line.
  ;; Returns (values vb glyphs) where glyphs is (vectorof glyph?) for caching.
  ;;
  ;; Face-id resolution:
  ;;   1. Read base face-id from gap buffer: (face-ref gb byte-pos)
  ;;      face-id 0 = default, 1..255 = registered faces
  ;;   2. If position is in active region, merge with region overlay face-id
  ;;      via face-id-with-overlay-id.
  ;;   3. Cache merged face-ids per (base-fid . overlay-fid) pair.
  (define content     (visual-line-content vl))
  (define buf-pos     (visual-line-buf-pos vl))
  (define truncated?  (visual-line-truncated? vl))
  (define max-cols    (vbuffer-cols vb))
  (define gap-len     (gap-length gb))

  ;; Pre-resolve region overlay face-id (once per visual-line)
  (define region-fid
    (and face-cache region-beg region-end
         (< region-beg region-end)
         (face-id-for-name 'region face-cache)))

  ;; Cache: (base-fid . overlay-fid) → merged-fid
  (define merge-cache (make-hash))

  ;; Collect glyphs for row-cache (reversed, then reversed at end)
  (define glyphs-rev '())

  (let loop ([col      start-col]
             [char-idx 0]
             [byte-pos buf-pos])
    (when (< char-idx (string-length content))
      (define ch (string-ref content char-idx))

      ;; ── Face-id resolution ──
      ;; Read base face-id directly from gap buffer.
      (define base-fid
        (if (< byte-pos gap-len)
            (face-ref gb byte-pos)
            0))

      ;; Check if this byte is in the active region.
      (define in-region?
        (and region-fid
             (>= byte-pos region-beg)
             (<  byte-pos region-end)))

      ;; Resolve final face-id: merge base + overlay if in region.
      (define fid
        (cond [(not in-region?) base-fid]
              [(zero? region-fid) base-fid]
              [(zero? base-fid) region-fid]
              [else
               (define key (cons base-fid region-fid))
               (hash-ref! merge-cache key
                 (λ ()
                   (face-id-with-overlay-id
                    base-fid region-fid face-cache)))]))
      ;; ── End face-id resolution ──

      ;; Character display width
      (define cw (max 0 (char-display-width ch)))

      ;; ── Character rendering ──
      (cond
        ;; Tab → spaces
        [(char=? ch #\tab)
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
           (define ctrl-ch (integer->char (+ 64 (char->integer ch))))
           (vbuffer-put-char! vb row (add1 col) ctrl-ch #:face-id fid)
           (set! glyphs-rev (cons (glyph ctrl-ch 1 fid) glyphs-rev)))
         (loop (+ col 2)
               (add1 char-idx)
               (gap-next-char-pos gb byte-pos))]

        ;; DEL character → ^?
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
         ;; Skip extra column for wide chars (display-width ≥ 2)
         (loop (+ col (if (>= cw 2) 2 1))
               (add1 char-idx)
               (gap-next-char-pos gb byte-pos))])))

  ;; Truncation marker '$'
  (when (and truncated? (> max-cols 0))
    (vbuffer-put-char! vb row (sub1 max-cols) #\$))

  (values vb (list->vector (reverse glyphs-rev))))
