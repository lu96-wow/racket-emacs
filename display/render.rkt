#lang racket

;; display/render.rkt — layout + gap face-ids → vbuffer window
;;
;; ============================================================================
;; Pure computation: walks layout visual-lines, reads face-ids directly
;; from the gap buffer (colocated with text), produces a vbuffer with
;; per-row buffer byte-range metadata.  No terminal output, no ANSI.
;;
;; ============================================================================
;; Data Flow
;; ============================================================================
;;
;;   gap-buffer (bytes + faces)  ─┐
;;   layout (visual-lines)       ─┤
;;   face-registry (named face lookup)   ─┤
;;                                ├─→ render-visual-line! → vbuffer-row
;;                                │    (cells + buf-start/end + flags)
;;                                │
;;                                └─→ vbuffer (screen grid + gap ref)
;;
;; ============================================================================
;; Computation vs Application
;; ============================================================================
;;
;;   ── Pure (compute vbuffer from layout + gap) ──
;;     render-layout!  render-layout/region!
;;     render-layout/cached!  render-layout/region/cached!
;;
;;   The result vbuffer carries a reference to the gap-buffer for
;;   screen↔buffer position queries (vbuffer-xy->byte-pos, etc.).
;;
;; ============================================================================
;; UTF-8 Handling
;; ============================================================================
;;
;;   Wide characters (CJK, emoji): face-id from first byte, stored at
;;   column C with width=2.  Column C+1 gets a space with fid=0 as
;;   a skip marker.  vbuffer-xy->byte-pos correctly maps both columns
;;   to the same buffer byte.
;;
;;   Combining characters (width=0): stored at column C+1 (advance by 1).
;;   The base character stays at column C.  Terminal rendering limitation
;;   — proper grapheme cluster composition needs a shaping engine.
;;
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
 ;; ── full render ──
 render-layout!
 render-layout/region!

 ;; ── incremental render (with row-cache) ──
 render-layout/cached!
 render-layout/region/cached!)

;; ============================================================
;; render-layout! — fill vbuffer from layout (full render)
;; ============================================================

(define (render-layout! ly gb reg)
  (unless (layout? ly)
    (raise-argument-error 'render-layout! "layout?" ly))
  (define rows (layout-max-rows ly))
  (define cols (layout-max-cols ly))
  (define vb  (make-vbuffer rows cols gb))

  (for ([vl  (in-list (layout-lines ly))]
        [row (in-naturals)]
        #:when (< row rows))
    (define vrow (render-visual-line! vl cols gb reg #f #f))
    (vbuffer-fill-row! vb row
                       (vbuffer-row-cells vrow)
                       (vbuffer-row-buf-start vrow)
                       (vbuffer-row-buf-end vrow)
                       (vbuffer-row-continued? vrow)
                       (vbuffer-row-truncated? vrow)
                       (vbuffer-row-display-len vrow)))
  vb)

;; ============================================================
;; render-layout/region! — with region highlight overlay
;; ============================================================

(define (render-layout/region! ly gb reg region-beg region-end)
  (define rows (layout-max-rows ly))
  (define cols (layout-max-cols ly))
  (define vb  (make-vbuffer rows cols gb))

  (for ([vl  (in-list (layout-lines ly))]
        [row (in-naturals)]
        #:when (< row rows))
    (define vrow (render-visual-line! vl cols gb reg
                                       region-beg region-end))
    (vbuffer-fill-row! vb row
                       (vbuffer-row-cells vrow)
                       (vbuffer-row-buf-start vrow)
                       (vbuffer-row-buf-end vrow)
                       (vbuffer-row-continued? vrow)
                       (vbuffer-row-truncated? vrow)
                       (vbuffer-row-display-len vrow)))
  vb)

;; ============================================================
;; render-layout/cached! — incremental render with row-cache
;; ============================================================

(define (render-layout/cached! ly gb reg row-cache)
  (define rows (layout-max-rows ly))
  (define cols (layout-max-cols ly))
  (define vb  (make-vbuffer rows cols gb))
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
       (define vrow (render-visual-line! vl cols gb reg #f #f))
       (vbuffer-fill-row! vb row
                          (vbuffer-row-cells vrow)
                          (vbuffer-row-buf-start vrow)
                          (vbuffer-row-buf-end vrow)
                          (vbuffer-row-continued? vrow)
                          (vbuffer-row-truncated? vrow)
                          (vbuffer-row-display-len vrow))
       (row-cache-store! row-cache row vrow)]))

  (row-cache-clear-from! row-cache (length vlines))
  vb)

;; ============================================================
;; render-layout/region/cached! — with region + row-cache
;; ============================================================

(define (render-layout/region/cached! ly gb reg
                                       region-beg region-end row-cache)
  (define rows (layout-max-rows ly))
  (define cols (layout-max-cols ly))
  (define vb  (make-vbuffer rows cols gb))
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
       (define vrow (render-visual-line! vl cols gb reg
                                          region-beg region-end))
       (vbuffer-fill-row! vb row
                          (vbuffer-row-cells vrow)
                          (vbuffer-row-buf-start vrow)
                          (vbuffer-row-buf-end vrow)
                          (vbuffer-row-continued? vrow)
                          (vbuffer-row-truncated? vrow)
                          (vbuffer-row-display-len vrow))
       (row-cache-store! row-cache row vrow)]))

  (row-cache-clear-from! row-cache (length vlines))
  vb)

;; ============================================================
;; render-visual-line! — one visual-line → vbuffer-row
;; ============================================================

(define (render-visual-line! vl ncols gb reg region-beg region-end)
  ;; Fill one screen row.  Returns a vbuffer-row with cells + byte-range.
  (define content     (visual-line-content vl))
  (define buf-pos     (visual-line-buf-pos vl))
  (define buf-end     (visual-line-end-buf-pos vl))
  (define continued?  (visual-line-continued? vl))
  (define truncated?  (visual-line-truncated? vl))
  (define display-len (visual-line-display-len vl))
  (define gap-len     (gap-length gb))

  ;; Pre-resolve region overlay face-id (once per row)
  (define fc (and reg (face-registry-cache reg)))
  (define region-fid
    (and reg region-beg region-end
         (< region-beg region-end)
         (face-id-for-name reg (quote region))))

  ;; Cache: (base-fid . overlay-fid) → merged-fid
  (define merge-cache (make-hash))

  ;; Build cells vector
  (define cells (make-vector ncols (cell #\space #f 0)))

  (let loop ([col 0] [char-idx 0] [byte-pos buf-pos])
    (when (and (< char-idx (string-length content)) (< col ncols))
      (define ch (string-ref content char-idx))

      ;; Face-id resolution
      (define base-fid (if (< byte-pos gap-len) (face-ref gb byte-pos) 0))
      (define in-region? (and region-fid
                              (>= byte-pos region-beg)
                              (< byte-pos region-end)))
      (define fid
        (cond [(not in-region?) base-fid]
              [(zero? region-fid) base-fid]
              [(zero? base-fid) region-fid]
              [else
               (define key (cons base-fid region-fid))
               (hash-ref! merge-cache key
                          (λ ()
                            (face-id-with-overlay-id
                             base-fid region-fid fc)))]))

      (define cw (max 0 (char-display-width ch)))

      ;; Character rendering dispatch
      (cond
        [(char=? ch #\tab)
         (let* ([tab-w (tab-width)]
                [end-col (min ncols (+ col tab-w))])
           (for ([c (in-range col end-col)])
             (vector-set! cells c (cell #\space #f fid)))
           (loop end-col (add1 char-idx)
                 (gap-next-char-pos gb byte-pos)))]

        [(< (char->integer ch) 32)
         (when (< col ncols)
           (vector-set! cells col (cell #\^ #f fid)))
         (when (< (add1 col) ncols)
           (let ([ctrl-ch (integer->char (+ 64 (char->integer ch)))])
             (vector-set! cells (add1 col) (cell ctrl-ch #f fid))))
         (loop (+ col 2) (add1 char-idx)
               (gap-next-char-pos gb byte-pos))]

        [(char=? ch #\rubout)
         (when (< col ncols)
           (vector-set! cells col (cell #\^ #f fid)))
         (when (< (add1 col) ncols)
           (vector-set! cells (add1 col) (cell #\? #f fid)))
         (loop (+ col 2) (add1 char-idx)
               (gap-next-char-pos gb byte-pos))]

        [else
         (when (< col ncols)
           (vector-set! cells col (cell ch #f fid)))
         (let ([advance (if (>= cw 2) 2 1)])
           (loop (+ col advance) (add1 char-idx)
                 (gap-next-char-pos gb byte-pos)))])))

  ;; Truncation marker
  (when (and truncated? (> ncols 0))
    (vector-set! cells (sub1 ncols) (cell #\$ #f 0)))

  (vbuffer-row cells buf-pos buf-end continued? truncated? display-len))
