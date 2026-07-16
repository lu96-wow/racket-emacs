#lang racket

;; display/mouse.rkt — Mouse click → buffer position
;;
;; Pure: given frame geometry + screen coords, find which leaf
;; and compute buffer position.

(require "../kernel/buffer.rkt"
         "../kernel/text.rkt"
         "../kernel/gap/query.rkt"
         "char-width.rkt"
         "layout.rkt"
         "window.rkt")

(provide screen-coord->buffer-pos)

(define (screen-coord->buffer-pos frm row col)
  (define geo (frame-geometry frm))
  ;; Find leaf + rect in one pass, no second lookup
  (define hit
    (for/or ([(lf rect) (in-hash geo)])
      (and (<= (rect-top rect) row (+ (rect-top rect) (rect-rows rect) -1))
           (<= (rect-left rect) col (+ (rect-left rect) (rect-cols rect) -1))
           (cons lf rect))))
  (cond
    [(not hit) (values #f #f 'nothing)]
    [else
     (define lf   (car hit))
     (define rect (cdr hit))
     (define buf  (leaf-buffer lf))
     (unless buf (values #f #f 'nothing))
     (let* ([tx (buffer-text buf)]
            [gb (text-gap tx)]
            [win-row (- row (rect-top rect))]
            [win-col (- col (rect-left rect))]
            [start-pos (text-marker-pos tx (leaf-start lf))]
            [max-cols (rect-cols rect)]
            [left-col (leaf-hscroll lf)]
            [wrap-mode (if (truncate-lines? buf) 'none 'char)]
            [vlines (visual-line-lines gb start-pos (add1 win-row) max-cols
                                        #:wrap-mode wrap-mode #:left-col left-col)])
       (if (>= win-row (length vlines))
           (values (buffer-length buf) lf 'text)
           (let* ([vl (list-ref vlines win-row)]
                  [line-start (visual-line-buf-pos vl)]
                  [line-text  (visual-line-content vl)]
                  [line-end
                   (let loop ([p line-start] [n (string-length line-text)])
                     (if (zero? n) p
                         (let-values ([(ch clen) (gap-char+len gb p)])
                           (loop (+ p clen) (sub1 n)))))]
                  [target-pos (scan-display-width gb line-start line-end win-col)])
             (values target-pos lf 'text))))]))
