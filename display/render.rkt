#lang racket

;; display/render.rkt — Window rendering pipeline
;;
;; For each leaf: layout (already computed) → fill vbuffer → compose.
;; Incremental diff against cache, flush only changed rows.

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
 display-frame
 check-min-size! render-slot
 update-frame-size!
 invalidate-frame-cache!
 recenter-point!
 ;; re-export
 (all-from-out "layout.rkt")
 (all-from-out "mouse.rkt"))

;; ============================================================
;; Minimum size
;; ============================================================

(define MIN-ROWS 4)
(define MIN-COLS 15)
(define frame-dirty? (make-hasheq))
(define render-slot (box #f))

(define (render-too-small frm)
  (detect-terminal-size!)
  (define w (frame-width frm)) (define h (frame-height frm))
  (display format-cursor-hide)
  (define msg "window too small")
  (hash-remove! frame-dirty? frm)
  (display format-clear-screen)
  (when (and (>= h 1) (>= w (string-length msg)))
    (display (format-cursor-move (quotient h 2) 0))
    (display (make-string (max 0 (quotient (- w (string-length msg)) 2)) #\space))
    (display format-reverse) (display msg) (display format-reset))
  (display format-cursor-show) (flush-output))

(define (check-min-size! box frm)
  (set-box! box (if (and (>= (frame-width frm) MIN-COLS) (>= (frame-height frm) MIN-ROWS))
                    display-frame render-too-small)))

;; ============================================================
;; render-visual-lines!
;; ============================================================

(define (render-visual-lines! vb buf gb vlines pt-pos cols)
  (define reg-active? (region-active? buf))
  (define reg-beg (and reg-active? (region-beginning buf)))
  (define reg-end (and reg-active? (region-end buf)))
  ;; Face-id cache: (base-face . overlay) → id, avoids repeated merges
  (define face-id-cache (make-hash))
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
          (let ([base-face (buffer-face-at buf char-bp)]
                [overlay   (and reg-active? (>= char-bp reg-beg) (< char-bp reg-end)
                                region-face)])
            (if (or base-face overlay)
                (let ([key (cons base-face overlay)])
                  (or (hash-ref! face-id-cache key
                        (λ () (face-id-with-overlay base-face overlay)))
                      0))
                0)))
        (vbuffer-put-char! vb r col ch #:face-id fid)
        (+ col cw))
      (when (visual-line-truncated? vl) (vbuffer-put-char! vb r (sub1 cols) #\$))
      (if (and (>= pt-pos line-pos) (<= pt-pos (+ line-pos char-len)))
          (values r (gap-display-width gb line-pos pt-pos))
          (values cr cc))))
  (if c-row (values c-row c-col)
      (let ([rev (reverse vlines)])
        (if (null? rev) (values 0 0)
            (let ([lv (car rev)])
              (values (sub1 (length vlines)) (visual-line-display-len lv)))))))

;; ============================================================
;; render-leaf! — fill vbuffer for one leaf
;; ============================================================

(define (render-leaf! lf rows cols selected?)
  (define buf (leaf-buffer lf))
  (unless buf (error 'render-leaf! "leaf has no buffer"))
  (when (or (zero? rows) (zero? cols))
    (values (make-vbuffer 1 1) #f #f))
  (define tx (buffer-text buf))
  (define gb (text-gap tx))
  (define start-pos (if (leaf-start lf) (text-marker-pos tx (leaf-start lf)) 0))
  (define pt-pos (if selected? (buffer-point buf) (leaf-point lf)))
  (define left-col (leaf-hscroll lf))
  (define wrap-mode (if (truncate-lines? buf) 'none 'char))
  (define vb (make-vbuffer rows cols))
  (define-values (c-row c-col)
    (if (> rows 0)
        (let* ([vlines (visual-line-lines gb start-pos rows cols
                                          #:wrap-mode wrap-mode #:left-col left-col)])
          (render-visual-lines! vb buf gb vlines pt-pos cols))
        (values #f #f)))
  (values vb
          (and selected? c-row)
          (and selected? c-col)))

;; ============================================================
;; compose-frame!
;; ============================================================

(define (compose-frame! frm)
  (define fw (frame-width frm)) (define fh (frame-height frm))
  (define final-vb (make-vbuffer fh fw))
  (define geo (frame-geometry frm))
  (define sel (frame-selected frm))
  (define-values (cur-row cur-col)
    (for/fold ([cr #f] [cc #f]) ([(lf rect) (in-hash geo)])
      (let*-values ([(top left rows cols)
                     (values (rect-top rect) (rect-left rect)
                             (rect-rows rect) (rect-cols rect))]
                    [(sub-vb sr sc) (render-leaf! lf rows cols (eq? lf sel))])
        (vbuffer-blit! final-vb top left sub-vb)
        (if (and sr sc (eq? lf sel))
            (values (+ top sr) (+ left sc))
            (values cr cc)))))
  (values final-vb cur-row cur-col))

;; ============================================================
;; recenter-point!
;; ============================================================

(define (end-of-physical-lines gb start n)
  (define len (gap-length gb))
  (define (nl? b) (= b #x0A))
  (let loop ([pos start] [remaining n])
    (if (or (zero? remaining) (>= pos len)) pos
        (let ([nl (gap-scan-byte gb pos 'forward nl?)])
          (if (>= nl len) len (loop (add1 nl) (sub1 remaining)))))))

(define (beginning-of-nth-prev-line gb pos n)
  (define (nl? b) (= b #x0A))
  (let loop ([p pos] [remaining n])
    (if (<= p 0) 0
        (let ([nl (gap-scan-byte gb (max 0 (sub1 p)) 'backward nl?)])
          (if (< nl 0) 0
              (if (zero? remaining) (add1 nl) (loop nl (sub1 remaining))))))))

;; ============================================================
;; Scroll: pure calc + separate apply
;; ============================================================

;; calc-scroll — pure: compute new start-pos and hscroll
;; Returns (values start-pos hscroll) where #f means "no change"
(define (calc-scroll gb pt-pos start-pos rows cols hscroll selected? trunc?)
  (define len (gap-length gb))
  ;; Vertical: last visible buffer position
  (define last-buf-pos
    (if trunc?
        (end-of-physical-lines gb start-pos rows)
        (let ([vlines (visual-line-lines gb start-pos rows cols
                                         #:wrap-mode 'char #:left-col 0)])
          (if (null? vlines) start-pos
              (let* ([lv (last vlines)])
                (+ (visual-line-buf-pos lv) (string-length (visual-line-content lv))))))))
  ;; Vertical scroll decision
  (define-values (v-start v-hscroll)
    (cond [(< pt-pos start-pos)
           (define nl (gap-scan-byte gb pt-pos 'backward (λ (b) (= b #x0A))))
           (values (if (>= nl 0) (add1 nl) 0)
                   (if (> hscroll 0) 0 #f))]
          [(> pt-pos last-buf-pos)
           (define target-lines (max 1 (quotient (* rows 2) 3)))
           (values (beginning-of-nth-prev-line gb pt-pos target-lines)
                   (if (> hscroll 0) 0 #f))]
          [else (values #f #f)]))
  ;; Horizontal scroll decision (only for selected windows in truncate mode)
  (define h-new
    (if (and selected? trunc?)
        (let* ([bol (gap-scan-byte gb pt-pos 'backward (λ (b) (= b #x0A)))]
               [pt-col (gap-display-width gb (if (>= bol 0) (add1 bol) 0) pt-pos)]
               [hs (or v-hscroll hscroll)])
          (cond [(< pt-col hs)        pt-col]
                [(>= pt-col (+ hs cols)) (max 0 (- pt-col cols -1))]
                [else #f]))
        #f))
  (values (or v-start start-pos)
          (or h-new v-hscroll hscroll)))

;; apply-scroll! — write computed scroll to leaf
(define (apply-scroll! lf start-pos hscroll)
  (define tx (buffer-text (leaf-buffer lf)))
  (text-set-marker-pos! tx (leaf-start lf) start-pos)
  (set-leaf-hscroll! lf hscroll))

;; recenter-point! — compose calc + apply
(define (recenter-point! lf rect selected?)
  (define buf (leaf-buffer lf))
  (define gb (text-gap (buffer-text buf)))
  (define pt (if selected? (buffer-point buf) (leaf-point lf)))
  (define ws (text-marker-pos (buffer-text buf) (leaf-start lf)))
  (define rows (max 1 (rect-rows rect)))
  (define cols (rect-cols rect))
  (define-values (new-start new-hscroll)
    (calc-scroll gb pt ws rows cols (leaf-hscroll lf) selected? (truncate-lines? buf)))
  (apply-scroll! lf new-start new-hscroll))

;; ============================================================
;; update-frame-size! / cache
;; ============================================================

(define (update-frame-size! frm)
  (detect-terminal-size!)
  (define w (terminal-width)) (define h (terminal-height))
  (when (or (not (= w (frame-width frm))) (not (= h (frame-height frm))))
    (set-frame-width! frm w) (set-frame-height! frm h)
    (layout-frame! frm)
    (check-min-size! render-slot frm)))

(define frame-cache-table (make-hasheq))

(define (invalidate-frame-cache! [frm #f])
  (define f (or frm (current-frame)))
  (when f (hash-remove! frame-cache-table f)))

;; ============================================================
;; display-frame — main entry
;; ============================================================

(define (display-frame frm)
  (detect-terminal-size!)
  (update-frame-size! frm)
  (init-face-cache!)
  (when (hash-ref frame-dirty? frm #f)
    (display format-clear-screen)
    (hash-remove! frame-dirty? frm))
  ;; Recenter each leaf
  (define sel (frame-selected frm))
  (for ([(lf rect) (in-hash (frame-geometry frm))])
    (recenter-point! lf rect (eq? lf sel)))
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

(set-box! render-slot display-frame)
