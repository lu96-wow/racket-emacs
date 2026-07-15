#lang racket

;; display/render.rkt — Single-buffer terminal rendering
;;
;; One pass over the gap buffer: builds visual lines, finds cursor.
;; Incremental: caches previous content, only flushes changed rows.
;; Face support via optional (byte-pos -> face-attrs) callback.

(require "../kernel/buffer.rkt"
         "../kernel/text.rkt"
         "../kernel/gap/gap.rkt"
         "../kernel/gap/query.rkt"
         "../platform/ansi.rkt"
         "../platform/termios.rkt"
         "char-width.rkt"
         "face.rkt")

(provide
 display-buffer
 invalidate-display-cache!
 face-for-pos)  ; parameter for user to install

;; ============================================================
;; face-for-pos — parameter, set by user for syntax highlighting
;; ============================================================
;; Default: no face (use default face / ansi-reset).
;; Callers install a (byte-pos -> (or/c face-attrs? #f)) function.

(define face-for-pos (make-parameter (λ (pos) #f)))

;; ============================================================
;; Per-buffer cache
;; ============================================================
;; Cache: buffer → (vector rows cols line-strings cursor-row cursor-col)
;; Invalidated on resize or explicitly.

(define render-cache (make-hasheq))

(define (invalidate-display-cache! [buf #f])
  (if buf
      (hash-remove! render-cache buf)
      (set! render-cache (make-hasheq))))

;; ============================================================
;; Internal: one visual line
;; ============================================================

(struct vline (buf-start buf-end content disp-width) #:transparent)
;; buf-start  : byte-pos — first byte of this visual line
;; buf-end    : byte-pos — first byte after the newline (or buffer-end)
;; content    : string  — tab-expanded, truncated for display
;; disp-width : integer — display columns consumed

;; ============================================================
;; Internal: tab-stop helper
;; ============================================================

(define (tab-next-col col)
  (* (add1 (quotient col (tab-width))) (tab-width)))

;; ============================================================
;; Internal: build one logical line's visual content
;; ============================================================
;; Walks [start, end) in the gap buffer, expanding tabs,
;; truncating at max-cols.  Returns (values string width).

(define (build-line gb start end max-cols)
  (let loop ([pos start] [col 0] [out (open-output-string)])
    (cond
      [(>= pos end)
       (values (get-output-string out) col)]
      [(>= col max-cols)
       (display "$" out)
       (values (get-output-string out) max-cols)]
      [else
       (define-values (ch clen) (gap-char+len gb pos))
       (cond
         [(char=? ch #\newline)
          (values (get-output-string out) col)]
         [(char=? ch #\tab)
          (define target (tab-next-col col))
          (define spaces (min (- target col) (- max-cols col)))
          (display (make-string spaces #\space) out)
          (loop (+ pos clen) (+ col spaces) out)]
         [else
          (define cw (max 0 (char-display-width ch)))
          (if (> (+ col cw) max-cols)
              (begin
                (display "$" out)
                (values (get-output-string out) max-cols))
              (begin
                (display ch out)
                (loop (+ pos clen) (+ col cw) out)))])])))

;; ============================================================
;; Internal: display-column at a byte position within a line
;; ============================================================

(define (display-col-at gb line-start target-pos)
  (let loop ([pos line-start] [col 0])
    (if (>= pos target-pos)
        col
        (let*-values ([(ch clen) (gap-char+len gb pos)])
          (define cw (if (char=? ch #\tab)
                         (- (tab-next-col col) col)
                         (max 0 (char-display-width ch))))
          (loop (+ pos clen) (+ col cw))))))

;; ============================================================
;; Internal: single pass — build vlines + find cursor
;; ============================================================
;; Returns (values (listof vline) cursor-row cursor-col).

(define (scan-buffer gb pt-pos max-rows max-cols)
  (define buf-len (gap-length gb))

  (define-values (rev-lines cr cc)
    (let loop ([pos 0] [row 0] [lines '()] [cr #f] [cc #f])
      (if (or (>= row max-rows) (>= pos buf-len))
          (values lines cr cc)
          (let* ([nl-byte (gap-scan-byte gb pos 'forward
                                         (λ (b) (= b #x0A)))]
                 [line-end (min nl-byte buf-len)])
            (define-values (content dw)
              (build-line gb pos line-end max-cols))
            ;; Is point in this visible line?
            (define-values (new-cr new-cc)
              (if cr
                  (values cr cc)
                  (cond
                    [(< pt-pos pos)     (values #f #f)]
                    [(< pt-pos line-end)
                     (values row (display-col-at gb pos pt-pos))]
                    [(= pt-pos line-end)
                     (values row dw)]
                    [else (values #f #f)])))
            ;; Next line starts after the newline byte (or at buffer end)
            (define next-pos
              (if (< nl-byte buf-len) (add1 nl-byte) buf-len))
            (loop next-pos (add1 row)
                  (cons (vline pos line-end content dw) lines)
                  new-cr new-cc)))))

  (define final-cr (or cr (if (null? rev-lines) 0 (sub1 (length rev-lines)))))
  (define final-cc (or cc 0))
  (values (reverse rev-lines) final-cr final-cc))

;; ============================================================
;; display-buffer — main entry
;; ============================================================

(define (display-buffer buf)
  (define w (terminal-width))
  (define h (terminal-height))
  (define gb (text-gap (buffer-text buf)))
  (define pt (buffer-point buf))
  (define face-fn (face-for-pos))

  ;; Build current visual lines + cursor
  (define-values (vlines cr cc) (scan-buffer gb pt h w))

  ;; Clamp cursor to terminal bounds
  (set! cr (min (max cr 0) (sub1 h)))
  (set! cc (min (max cc 0) (sub1 w)))

  ;; Extract content strings
  (define cur-lines
    (list->vector (map vline-content vlines)))

  ;; Build face-annotated strings if face-for-pos is set
  (define cur-annotated
    (and face-fn
         (for/vector ([vl (in-list vlines)])
           (annotate-line gb (vline-buf-start vl) (vline-content vl) face-fn))))

  ;; Fetch previous cache
  (define prev (hash-ref render-cache buf #f))
  (define prev-rows (and prev (vector-ref prev 0)))
  (define prev-cols (and prev (vector-ref prev 1)))
  (define prev-lines (and prev (vector-ref prev 2)))
  (define prev-cr (and prev (vector-ref prev 3)))
  (define prev-cc (and prev (vector-ref prev 4)))

  (define cache-valid?
    (and prev (= prev-rows h) (= prev-cols w)))

  ;; ---- Flush ----
  (display format-cursor-hide)

  (for ([r (in-range h)])
    (define cur
      (if face-fn
          (and (< r (vector-length cur-annotated))
               (vector-ref cur-annotated r))
          (and (< r (vector-length cur-lines))
               (vector-ref cur-lines r))))
    (define prv
      (and cache-valid? (< r (vector-length prev-lines))
           (vector-ref prev-lines r)))
    (when (not (equal? cur prv))
      (display (format-cursor-move r 0))
      (if cur
          (begin
            (display format-reset)
            (display cur)
            (display format-clear-to-eol))
          (begin
            (display format-reset)
            (display format-clear-to-eol)))))

  ;; Move cursor if position changed
  (when (or (not cache-valid?)
            (not (= cr prev-cr))
            (not (= cc prev-cc)))
    (display (format-cursor-move cr cc)))

  (display format-cursor-show)
  (flush-output)

  ;; Update cache (store the simpler string version for comparison)
  (hash-set! render-cache buf
             (vector h w cur-lines cr cc)))

;; ============================================================
;; Internal: annotate a line with face escape sequences
;; ============================================================
;; Given a logical line content and its start byte position,
;; insert face ANSI bytes at position boundaries.

(define (annotate-line gb line-start content face-fn)
  (define out (open-output-string))
  (define current-bs #"")
  (let loop ([pos line-start] [ci 0])
    (cond
      [(>= ci (string-length content))
       ;; End of line — close any open face
       (unless (equal? current-bs #"")
         (display format-reset out))
       (get-output-string out)]
      [else
       (define attrs (face-fn pos))
       (define this-bs
         (if attrs (realize-face attrs (color-depth)) #""))
       ;; Face changed → emit reset + new face
       (unless (equal? this-bs current-bs)
         (display format-reset out)
         (when (positive? (bytes-length this-bs))
           (display this-bs out))
         (set! current-bs this-bs))
       (display (string-ref content ci) out)
       (define-values (_ch clen) (gap-char+len gb pos))
       (loop (+ pos clen) (add1 ci))])))
