#lang racket

;; display/render.rkt — Display pipeline with dirty-flag-driven lazy redisplay
;;
;; Three clearly separated phases:
;;   prepare  — decide which leaves need update (dirty-flag + row-cache match)
;;   generate — build vbuffer for each dirty leaf
;;   flush    — delta diff + terminal output
;;
;; Pure calc functions produce data; apply functions mutate state.
;; The render function itself is called by the event loop at safe points,
;; not after every event.

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
         "scratch.rkt"
         "dirty.rkt"
         "mouse.rkt")

(provide
 ;; main entry
 display-frame
 render-slot

 ;; dirty flags (re-export)
 (all-from-out "dirty.rkt")

 ;; size
 update-frame-size!
 check-min-size!

 ;; cache
 invalidate-frame-cache!
 invalidate-leaf-cache!

 ;; recenter
 recenter-point!

 ;; re-export
 (all-from-out "layout.rkt")
 (all-from-out "mouse.rkt"))

;; ============================================================
;; Minimum size
;; ============================================================

(define MIN-ROWS 4)
(define MIN-COLS 15)
(define render-slot (box #f))

(define (render-too-small frm)
  (detect-terminal-size!)
  (display format-cursor-hide)
  (display format-clear-screen)
  (define msg "window too small")
  (when (and (>= (frame-height frm) 1)
             (>= (frame-width frm) (string-length msg)))
    (display (format-cursor-move (quotient (frame-height frm) 2) 0))
    (display (make-string (max 0 (quotient (- (frame-width frm) (string-length msg)) 2)) #\space))
    (display format-reverse) (display msg) (display format-reset))
  (display format-cursor-show) (flush-output))

(define (check-min-size!)
  (define frm (current-frame))
  (when frm
    (set-box! render-slot
      (if (and (>= (frame-width frm) MIN-COLS) (>= (frame-height frm) MIN-ROWS))
          display-frame
          render-too-small))))

;; ============================================================
;; Per-leaf row caches
;; ============================================================

(define leaf-cache-table (make-hasheq))

(define (leaf-row-cache lf)
  (hash-ref leaf-cache-table lf (λ ()
    (define c (make-row-cache 200))  ; generous max rows
    (hash-set! leaf-cache-table lf c)
    c)))

(define (invalidate-leaf-cache! lf)
  (define cache (hash-ref leaf-cache-table lf #f))
  (when cache (row-cache-invalidate! cache)))

(define (invalidate-frame-cache! [frm #f])
  (define f (or frm (current-frame)))
  (when f
    (for ([lf (in-list (frame-leaf-list f))])
      (invalidate-leaf-cache! lf))
    (frame-cache-invalidate! f)))

;; ============================================================
;; Frame-level vbuffer cache (for delta flush)
;; ============================================================

(define frame-cache-table (make-hasheq))

(define (frame-cache-invalidate! frm)
  (hash-remove! frame-cache-table frm))

;; ============================================================
;; render-leaf! — fill vbuffer for one leaf, using row cache
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
  (define content-rows (max 1 rows))  ; todo: mode-line would use rows-1

  ;; ---- Generate visual lines ----
  (define vlines (visual-line-lines gb start-pos content-rows cols
                                    #:wrap-mode wrap-mode #:left-col left-col))

  ;; ---- Update row cache ----
  (define cache (leaf-row-cache lf))
  (for ([vl (in-list vlines)] [r (in-naturals)])
    (define line-pos  (visual-line-buf-pos vl))
    (define line-end  (+ line-pos (string-length (visual-line-content vl))))
    (define glyphs
      (for/vector ([ch (in-string (visual-line-content vl))])
        (define cw (max 1 (char-display-width ch)))
        (glyph ch cw 0)))  ; face-id=0 for now
    (row-cache-update! cache r line-pos line-end glyphs
                       (visual-line-continued? vl)
                       (visual-line-truncated? vl)))
  ;; Clear remaining cache rows
  (row-cache-clear-from! cache (length vlines))

  ;; ---- Fill vbuffer from visual lines ----
  (define face-id-cache (make-hash))
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
      (when (visual-line-truncated? vl)
        (vbuffer-put-char! vb r (sub1 cols) #\$))
      (if (and (>= pt-pos line-pos) (<= pt-pos (+ line-pos char-len)))
          (values r (gap-display-width gb line-pos pt-pos))
          (values cr cc))))
  (if c-row (values vb c-row c-col)
      (let ([rev (reverse vlines)])
        (if (null? rev) (values vb 0 0)
            (let ([lv (car rev)])
              (values vb (sub1 (length vlines)) (visual-line-display-len lv)))))))

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
;; Scroll helpers (from rebuild)
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

(define (calc-scroll gb pt-pos start-pos rows cols hscroll selected? trunc?)
  (define len (gap-length gb))
  (define last-buf-pos
    (if trunc?
        (end-of-physical-lines gb start-pos rows)
        (let ([vlines (visual-line-lines gb start-pos rows cols
                                         #:wrap-mode 'char #:left-col 0)])
          (if (null? vlines) start-pos
              (let* ([lv (last vlines)])
                (+ (visual-line-buf-pos lv) (string-length (visual-line-content lv))))))))
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

(define (apply-scroll! lf start-pos hscroll)
  (define tx (buffer-text (leaf-buffer lf)))
  (text-set-marker-pos! tx (leaf-start lf) start-pos)
  (set-leaf-hscroll! lf hscroll))

(define (recenter-point! lf rect selected?)
  (define buf (leaf-buffer lf))
  (define gb (text-gap (buffer-text buf)))
  (define pt (if selected? (buffer-point buf) (leaf-point lf)))
  (define ws (text-marker-pos (buffer-text buf) (leaf-start lf)))
  (define rows (max 1 (rect-rows rect)))
  (define cols (rect-cols rect))
  (define-values (new-start new-hscroll)
    (calc-scroll gb pt ws rows cols (leaf-hscroll lf) selected? (truncate-lines? buf)))
  (apply-scroll! lf new-start new-hscroll)
  ;; If scroll changed, invalidate cache
  (when (or (not (= new-start ws)) (not (= new-hscroll (leaf-hscroll lf))))
    (invalidate-leaf-cache! lf)))

;; ============================================================
;; update-frame-size!
;; ============================================================

(define (update-frame-size! frm)
  (detect-terminal-size!)
  (define w (terminal-width)) (define h (terminal-height))
  (when (or (not (= w (frame-width frm))) (not (= h (frame-height frm))))
    (set-frame-width! frm w) (set-frame-height! frm h)
    (layout-frame! frm)
    (invalidate-frame-cache! frm)
    (check-min-size!)))

;; ============================================================
;; display-frame — main entry
;; ============================================================

(define (display-frame frm)
  (detect-terminal-size!)
  (update-frame-size! frm)
  (init-face-cache!)

  ;; Phase 1: prepare — recenter, decide what needs update
  (define sel (frame-selected frm))
  (for ([(lf rect) (in-hash (frame-geometry frm))])
    (recenter-point! lf rect (eq? lf sel)))

  ;; Phase 2: generate + compose
  (define-values (new-vb cr cc) (compose-frame! frm))

  ;; Phase 3: flush
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
