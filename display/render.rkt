#lang racket

;; display/render.rkt — Multi-window rendering pipeline
;;
;; For each leaf window: render buffer content into sub-vbuffer,
;; then compose all windows into frame vbuffer.
;; Incremental: diff with cached vbuffer, only flush changed rows.
;; Ported from original display/render.rkt.

(require "../kernel/buffer.rkt"
         "../kernel/text.rkt"
         "../kernel/gap/gap.rkt"
         "../kernel/gap/query.rkt"
         "../kernel/vbuffer/vbuffer.rkt"
         "../platform/ansi.rkt"
         "../platform/termios.rkt"
         "char-width.rkt"
         "face.rkt"
         "window.rkt"
         "registry.rkt")

(provide
 display-frame display-buffer flush-vbuffer!
 pos->row-col
 recenter-point! update-frame-size!
 invalidate-frame-cache!
 screen-coord->buffer-pos
 buffer-wrap-mode set-buffer-wrap-mode!)

;; ============================================================
;; visual-line
;; ============================================================

(struct visual-line (buf-pos content continued? truncated? display-len)
  #:transparent)

;; ============================================================
;; truncate-lines — one visual-line per logical line, '$' for overflow
;; ============================================================

(define (truncate-lines gb start-pos max-rows max-cols left-col)
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
                              (max 0 (char-display-width ch)))])
          (loop (if (< line-end len) (add1 line-end) len)
                (add1 row)
                (cons (visual-line seg-start content #f trunc? display-len) acc))))))

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
            (define vl (visual-line seg-pos content cont? #f display-len))
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
;; Per-buffer state for wrap/hscroll
;; ============================================================

(define wrap-mode-table (make-hasheq))
(define hscroll-table  (make-hasheq))

(define (buffer-wrap-mode buf) (hash-ref wrap-mode-table buf 'none))
(define (set-buffer-wrap-mode! buf m) (hash-set! wrap-mode-table buf m))
(define (truncate-lines? [buf (current-buffer)])
  (eq? (buffer-wrap-mode buf) 'none))

(define (buffer-hscroll buf) (hash-ref hscroll-table buf 0))
(define (set-buffer-hscroll! buf n) (hash-set! hscroll-table buf (max 0 n)))

(define current-buffer (make-parameter #f))
(define (set-buffer buf) (current-buffer buf) buf)

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
      ;; Fill cells for this visual line
      (for/fold ([col 0]) ([ch (in-string line-str)] [char-idx (in-naturals)])
        (define cw (max 1 (char-display-width ch)))
        ;; Byte position of this character (for face lookup)
        (define char-bp
          (let loop2 ([p line-pos] [n char-idx])
            (if (zero? n) p
                (let-values ([(c len) (gap-char+len gb p)])
                  (loop2 (+ p len) (sub1 n))))))
        (define fid
          (if (and reg-active? (>= char-bp reg-beg) (< char-bp reg-end))
              3   ;; region face id (placeholder)
              0)) ;; default face id
        (vbuffer-put-char! vb r col ch #:face-id fid)
        (+ col cw))
      ;; Truncation marker
      (when (visual-line-truncated? vl)
        (vbuffer-put-char! vb r (sub1 cols) #\$))
      ;; Cursor in this line?
      (if (and (>= pt-pos line-pos) (<= pt-pos (+ line-pos char-len)))
          (values r (gap-display-width gb line-pos pt-pos))
          (values cr cc))))
  ;; Fallback
  (if c-row
      (values c-row c-col)
      (let ([rev-lines (reverse vlines)])
        (if (null? rev-lines)
            (values 0 0)
            (values (sub1 (length vlines)) 0)))))

;; ============================================================
;; pos->row-col
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
;; render-mode-line!
;; ============================================================

(define (render-mode-line! vb row cols buf pt-row pt-col)
  (define mod-flag (cond [(buffer-read-only? buf) "%%%%"]
                         [(buffer-modified? buf)   "**"]
                         [else                     "--"]))
  (define ml-left (format "-~a:---  ~a" mod-flag (buffer-name buf)))
  (define pos-info (format "L~a C~a  " (add1 (or pt-row 0)) pt-col))
  (define ll (string-length ml-left))
  (define rl (string-length pos-info))
  (define pad (max 0 (- cols ll rl)))
  (vbuffer-put-string! vb row 0
    (substring (string-append ml-left (make-string pad #\-) pos-info) 0 cols) 'reverse))

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
  (define content-rows (max 0 (sub1 rows)))
  (define left-col (window-hscroll w))
  (define wrap-mode (if (truncate-lines? buf) 'none 'char))
  (define vb (make-vbuffer rows cols))
  (define-values (c-row c-col)
    (if (> content-rows 0)
        (let* ([vlines (visual-line-lines gb start-pos content-rows cols
                                          #:wrap-mode wrap-mode #:left-col left-col)])
          (render-visual-lines! vb buf gb vlines pt-pos cols content-rows))
        (values #f #f)))
  (render-mode-line! vb content-rows cols buf c-row c-col)
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
;; recenter-point!
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
  (define content-rows (max 1 (sub1 (window-rows w))))
  (define cols (window-cols w))
  (define last-buf-pos
    (if (truncate-lines? buf)
        (end-of-physical-lines gb ws content-rows)
        (let ([vlines (visual-line-lines gb ws content-rows cols
                                         #:wrap-mode 'char #:left-col 0)])
          (if (null? vlines) ws
              (let* ([lv (last vlines)])
                (+ (visual-line-buf-pos lv)
                   (string-length (visual-line-content lv))))))))
  ;; Vertical scroll
  (cond
    [(< pt ws)
     (define nl (gap-scan-byte gb pt 'backward (λ (b) (= b #x0A))))
     (text-set-marker-pos! tx (window-start w) (if (>= nl 0) (add1 nl) 0))
     (when (> (window-hscroll w) 0) (set-window-hscroll! w 0))]
    [(> pt last-buf-pos)
     (define target-lines (max 1 (quotient (* content-rows 2) 3)))
     (text-set-marker-pos! tx (window-start w)
                           (beginning-of-nth-prev-line gb pt target-lines))
     (when (> (window-hscroll w) 0) (set-window-hscroll! w 0))]
    [else (void)])
  ;; Horizontal scroll
  (when (and (window-selected? w) (truncate-lines? buf))
    (define pt-col
      (let ([bol (gap-scan-byte gb pt 'backward (λ (b) (= b #x0A)))])
        (gap-display-width gb (if (>= bol 0) (add1 bol) 0) pt)))
    (define hs (window-hscroll w))
    (cond
      [(< pt-col hs)        (set-window-hscroll! w pt-col)]
      [(>= pt-col (+ hs cols)) (set-window-hscroll! w (max 0 (- pt-col cols -1)))]
      [else (void)])))

;; ============================================================
;; Frame cache — per-frame vbuffer for delta rendering
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
  ;; Recenter
  (for ([w (in-list leaves)]) (recenter-point! w))
  ;; Build desired matrix
  (define-values (new-vb cr cc) (compose-frame! frm))
  ;; Delta flush
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
;; flush-vbuffer-delta! — row-by-row diff
;; ============================================================

(define (flush-vbuffer-delta! new-vb cache)
  (define new-cells (vbuffer-cells new-vb))
  (define cols (vbuffer-cols new-vb))
  (define rows (vbuffer-rows new-vb))
  (define old-cells (and cache
                         (= cols (vbuffer-cols cache))
                         (= rows (vbuffer-rows cache))
                         (vbuffer-cells cache)))
  (define faces-by-id (face-cache-by-id (current-face-cache)))
  (define out (open-output-string))
  (for ([r (in-range rows)])
    (define row-start (* r cols))
    (define row-changed?
      (or (not old-cells)
          (for/or ([c (in-range cols)])
            (define i (+ row-start c))
            (define nc (vector-ref new-cells i))
            (define oc (vector-ref old-cells i))
            (not (and (char=? (cell-ch nc) (cell-ch oc))
                      (= (cell-face-id nc) (cell-face-id oc))
                      (equal? (cell-attrs nc) (cell-attrs oc)))))))
    (when row-changed?
      (display (format-cursor-move r 0) out)
      (flush-row-cells! out cols new-cells row-start faces-by-id)
      (display "\e[K" out)))
  (display (get-output-string out)))

;; ============================================================
;; flush-row-cells! — output one row with face/attr state machine
;; ============================================================

(define (flush-row-cells! out cols cells row-start faces-by-id)
  (define active-attrs #f)
  (define active-face-id 0)
  (define skip-next? #f)
  (for ([c (in-range cols)])
    (define cl (vector-ref cells (+ row-start c)))
    (define ch (cell-ch cl))
    (cond
      [skip-next? (set! skip-next? #f)]
      [else
       (define new-attrs (cell-attrs cl))
       (define new-syms (cond [(not new-attrs) '()]
                              [(symbol? new-attrs) (list new-attrs)]
                              [(list? new-attrs) new-attrs]
                              [else '()]))
       (define old-syms (if active-attrs active-attrs '()))
       (define face-changed? (not (= (cell-face-id cl) active-face-id)))
       (define attrs-changed? (not (equal? new-syms old-syms)))
       (cond
         [face-changed?
          (display format-reset out)
          (define rf (vector-ref faces-by-id (cell-face-id cl)))
          (define face-ansi (realized-face-ansi-bytes rf))
          (when (positive? (bytes-length face-ansi)) (display face-ansi out))
          (set! active-face-id (cell-face-id cl))
          (set! active-attrs (if (null? new-syms) #f new-syms))
          (unless (null? new-syms)
            (define bs (attrs->bytes new-attrs))
            (when bs (display bs out)))]
         [attrs-changed?
          (set! active-attrs (if (null? new-syms) #f new-syms))
          (if (null? new-syms)
              (display format-reset out)
              (let ([bs (attrs->bytes new-attrs)])
                (when bs (display bs out))))]
         [else (void)])
       (display ch out)
       (when (= (char-display-width ch) 2) (set! skip-next? #t))]))
  (when (or active-attrs (not (= active-face-id 0)))
    (display format-reset out)))

;; ============================================================
;; attrs helper
;; ============================================================

(define attr->format
  (hasheq 'reverse   format-reverse
          'bold      format-bold
          'dim       format-dim
          'italic    format-italic
          'underline format-underline
          'blink     format-blink))

(define (attrs->bytes attrs)
  (define syms (cond [(not attrs) '()]
                     [(symbol? attrs) (list attrs)]
                     [(list? attrs) attrs]
                     [else '()]))
  (define strs (for/list ([s (in-list syms)]
                          #:when (hash-has-key? attr->format s))
                 (hash-ref attr->format s)))
  (if (null? strs) #f (string->bytes/utf-8 (string-join strs ""))))

;; ============================================================
;; flush-vbuffer! — full output (for testing)
;; ============================================================

(define (flush-vbuffer! vb)
  (flush-vbuffer-delta! vb #f))

;; ============================================================
;; screen-coord->buffer-pos — mouse click → buffer position
;; ============================================================

(define (screen-coord->buffer-pos frm row col)
  (define leaves (frame-window-list frm))
  (define hit
    (for/or ([w (in-list leaves)])
      (and (<= (window-top w) row (+ (window-top w) (window-rows w) -1))
           (<= (window-left w) col (+ (window-left w) (window-cols w) -1))
           w)))
  (cond
    [(not hit) (values #f #f 'nothing)]
    [(not (window-start hit)) (values #f hit 'minibuffer)]
    [(= row (+ (window-top hit) (window-rows hit) -1)) (values #f hit 'mode-line)]
    [else
     (define buf (window-buffer hit))
     (define tx  (buffer-text buf))
     (define gb  (text-gap tx))
     (define win-row (- row (window-top hit)))
     (define win-col (- col (window-left hit)))
     (define start-pos (text-marker-pos tx (window-start hit)))
     (define max-cols (window-cols hit))
     (define left-col (window-hscroll hit))
     (define wrap-mode (if (truncate-lines? buf) 'none 'char))
     (define vlines (visual-line-lines gb start-pos (add1 win-row) max-cols
                                       #:wrap-mode wrap-mode #:left-col left-col))
     (if (>= win-row (length vlines))
         (values (buffer-length buf) hit 'text)
         (let* ([vl (list-ref vlines win-row)]
                [line-start (visual-line-buf-pos vl)]
                [line-text  (visual-line-content vl)]
                [line-end
                 (let loop ([p line-start] [n (string-length line-text)])
                   (if (zero? n) p
                       (let-values ([(ch clen) (gap-char+len gb p)])
                         (loop (+ p clen) (sub1 n)))))]
                [target-pos (scan-display-width gb line-start line-end win-col)])
           (values target-pos hit 'text)))]))

;; ============================================================
;; display-buffer — convenience entry for single-buffer view
;; ============================================================

(define (display-buffer buf)
  (define frm (current-frame))
  (if frm
      (display-frame frm)
      (let ([frm* (init-root-frame buf (terminal-width) (terminal-height))])
        (invalidate-frame-cache! frm*)
        (display-frame frm*))))
