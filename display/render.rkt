#lang racket

;; display/render.rkt — Window rendering pipeline
;;
;; For each leaf window: layout → fill vbuffer → compose into frame.
;; Incremental: diff frame vbuffer against cache, flush only changed rows.
;;
;; Dependencies: layout, flush, window, mouse, kernel/*, platform/*

(require "../kernel/buffer.rkt"
         "../kernel/text.rkt"
         "../kernel/gap/gap.rkt"
         "../kernel/gap/query.rkt"
         "../kernel/vbuffer/vbuffer.rkt"
         "../platform/ansi.rkt"
         "../platform/termios.rkt"
         "char-width.rkt"
         "face.rkt"
         "layout.rkt"
         "flush.rkt"
         "window.rkt"
         "registry.rkt"
         "mouse.rkt")

(provide
 display-frame display-buffer
 update-frame-size!
 invalidate-frame-cache!
 ;; re-export
 (all-from-out "layout.rkt")
 (all-from-out "mouse.rkt"))

;; ============================================================
;; render-visual-lines! — fill vbuffer from visual lines + find cursor
;; ============================================================

(define (render-visual-lines! vb buf gb vlines pt-pos cols content-rows)
  (define reg-active? (region-active? buf))
  (define reg-beg (and reg-active? (region-beginning buf)))
  (define reg-end (and reg-active? (region-end buf)))
  (define-values (c-row c-col)
    (for/fold ([cr #f] [cc #f]) ([vl (in-list vlines)] [r (in-naturals)])
      (define line-pos  (visual-line-buf-pos vl))
      (define line-str  (visual-line-content vl))
      (define str-len   (string-length line-str))
      (define char-len
        (let loop ([p line-pos] [n str-len])
          (if (zero? n) 0
              (let-values ([(ch clen) (gap-char+len gb p)])
                (+ clen (loop (+ p clen) (sub1 n)))))))
      (for/fold ([col 0]) ([ch (in-string line-str)] [char-idx (in-naturals)])
        (define cw (max 1 (char-display-width ch)))
        (define char-bp
          (let loop2 ([p line-pos] [n char-idx])
            (if (zero? n) p
                (let-values ([(c len) (gap-char+len gb p)])
                  (loop2 (+ p len) (sub1 n))))))
        (define fid
          (if (and reg-active? (>= char-bp reg-beg) (< char-bp reg-end)) 3 0))
        (vbuffer-put-char! vb r col ch #:face-id fid)
        (+ col cw))
      (when (visual-line-truncated? vl)
        (vbuffer-put-char! vb r (sub1 cols) #\$))
      (if (and (>= pt-pos line-pos) (<= pt-pos (+ line-pos char-len)))
          (values r (gap-display-width gb line-pos pt-pos))
          (values cr cc))))
  (if c-row
      (values c-row c-col)
      ;; Cursor past visible content: place at end of last visible line
      (let ([rev-lines (reverse vlines)])
        (if (null? rev-lines)
            (values 0 0)
            (let ([last-vl (car rev-lines)])
              (values (sub1 (length vlines))
                      (visual-line-display-len last-vl)))))))

;; ============================================================
;; render-window!
;; ============================================================

(define (render-window! w [force-cursor? #f])
  (define buf (window-buffer w))
  (unless buf (error 'render-window! "window has no buffer"))
  (define rows (window-rows w))
  (define cols (window-cols w))
  (when (or (zero? rows) (zero? cols))
    (values (make-vbuffer 1 1) #f #f))
  (define tx  (buffer-text buf))
  (define gb  (text-gap tx))
  (define start-pos (if (window-start w) (text-marker-pos tx (window-start w)) 0))
  (define pt-pos    (if (window-selected? w) (buffer-point buf) (window-point w)))
  (define left-col (window-hscroll w))
  (define wrap-mode (if (truncate-lines? buf) 'none 'char))
  (define vb (make-vbuffer rows cols))
  (define-values (c-row c-col)
    (if (> rows 0)
        (let* ([vlines (visual-line-lines gb start-pos rows cols
                                          #:wrap-mode wrap-mode #:left-col left-col)])
          (render-visual-lines! vb buf gb vlines pt-pos cols rows))
        (values #f #f)))
  (values vb
          (and (or force-cursor? (window-selected? w)) c-row)
          (and (or force-cursor? (window-selected? w)) c-col)))

;; ============================================================
;; compose-frame!
;; ============================================================

(define (compose-frame! frm)
  (define fw (frame-width frm))
  (define fh (frame-height frm))
  (define final-vb (make-vbuffer fh fw))
  (define leaves (frame-window-list frm))
  (define-values (cur-row cur-col)
    (for/fold ([cr #f] [cc #f]) ([w (in-list leaves)])
      (let-values ([(sub-vb sr sc) (render-window! w)])
        (vbuffer-blit! final-vb (window-top w) (window-left w) sub-vb)
        (if (and (window-selected? w) sr sc)
            (values (+ (window-top w) sr) (+ (window-left w) sc))
            (values cr cc)))))
  (values final-vb cur-row cur-col))

;; ============================================================
;; recenter-point! — auto-scroll window to keep point visible
;; ============================================================

(define (end-of-physical-lines gb start n)
  (define len (gap-length gb))
  (define (nl? b) (= b #x0A))
  (let loop ([pos start] [remaining n])
    (if (or (zero? remaining) (>= pos len))
        pos
        (let ([nl (gap-scan-byte gb pos 'forward nl?)])
          (if (>= nl len) len (loop (add1 nl) (sub1 remaining)))))))

(define (beginning-of-nth-prev-line gb pos n)
  (define (nl? b) (= b #x0A))
  (let loop ([p pos] [remaining n])
    (if (<= p 0) 0
        (let ([nl (gap-scan-byte gb (max 0 (sub1 p)) 'backward nl?)])
          (if (< nl 0) 0
              (if (zero? remaining) (add1 nl) (loop nl (sub1 remaining))))))))

(define (recenter-point! w)
  (define buf (window-buffer w))
  (define tx  (buffer-text buf))
  (define gb  (text-gap tx))
  (define len (gap-length gb))
  (define pt (if (window-selected? w) (buffer-point buf) (window-point w)))
  (define ws (text-marker-pos tx (window-start w)))
  (define rows (max 1 (window-rows w)))
  (define cols (window-cols w))
  (define last-buf-pos
    (if (truncate-lines? buf)
        (end-of-physical-lines gb ws rows)
        (let ([vlines (visual-line-lines gb ws rows cols
                                         #:wrap-mode 'char #:left-col 0)])
          (if (null? vlines) ws
              (let* ([lv (last vlines)])
                (+ (visual-line-buf-pos lv)
                   (string-length (visual-line-content lv))))))))
  (cond
    [(< pt ws)
     (define nl (gap-scan-byte gb pt 'backward (λ (b) (= b #x0A))))
     (text-set-marker-pos! tx (window-start w) (if (>= nl 0) (add1 nl) 0))
     (when (> (window-hscroll w) 0) (set-window-hscroll! w 0))]
    [(> pt last-buf-pos)
     (define target-lines (max 1 (quotient (* rows 2) 3)))
     (text-set-marker-pos! tx (window-start w)
                           (beginning-of-nth-prev-line gb pt target-lines))
     (when (> (window-hscroll w) 0) (set-window-hscroll! w 0))]
    [else (void)])
  (when (and (window-selected? w) (truncate-lines? buf))
    (define pt-col
      (let ([bol (gap-scan-byte gb pt 'backward (λ (b) (= b #x0A)))])
        (gap-display-width gb (if (>= bol 0) (add1 bol) 0) pt)))
    (define hs (window-hscroll w))
    ;; Horizontal auto-scroll.
    ;; Threshold: scroll right when pt-col reaches or exceeds hs+cols.
    ;; target = pt-col - cols - 1  keeps 1 column of breathing room
    ;; on the right edge.  Using 1 (not 0) ensures CJK wide characters
    ;; (display-width 2) are not clipped mid-glyph at the boundary.
    (cond
      [(< pt-col hs)        (set-window-hscroll! w pt-col)]
      [(>= pt-col (+ hs cols)) (set-window-hscroll! w (max 0 (- pt-col cols -1)))]
      [else (void)])))

;; ============================================================
;; update-frame-size!
;; ============================================================

(define (update-frame-size! frm)
  (detect-terminal-size!)
  (define w (terminal-width))
  (define h (terminal-height))
  (when (or (not (= w (frame-width frm))) (not (= h (frame-height frm))))
    (set-frame-width! frm w)
    (set-frame-height! frm h)
    (layout-frame! frm)))

;; ============================================================
;; Frame cache
;; ============================================================

(define frame-cache-table (make-hasheq))

(define (invalidate-frame-cache! [frm #f])
  (define f (or frm (current-frame)))
  (when f (hash-remove! frame-cache-table f)))

;; ============================================================
;; display-frame — main entry
;; ============================================================

(define (display-frame frm)
  (detect-terminal-size!)
  (init-face-cache!)
  (define leaves (filter window-leaf? (frame-window-list frm)))
  (for ([w (in-list leaves)]) (recenter-point! w))
  (define-values (new-vb cr cc) (compose-frame! frm))
  (define cache (hash-ref frame-cache-table frm #f))
  (display format-cursor-hide)
  (flush-vbuffer-delta! new-vb cache)
  (when (and cr cc)
    (display (format-cursor-move (min cr (sub1 (frame-height frm)))
                                  (min cc (sub1 (frame-width frm))))))
  (display format-cursor-show)
  (hash-set! frame-cache-table frm new-vb)
  (flush-output))

;; ============================================================
;; display-buffer — convenience entry
;; ============================================================

(define (display-buffer buf)
  (define frm (current-frame))
  (if frm
      (display-frame frm)
      (let ([frm* (init-root-frame buf (terminal-width) (terminal-height))])
        (invalidate-frame-cache! frm*)
        (display-frame frm*))))
