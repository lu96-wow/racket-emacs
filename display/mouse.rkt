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
  (define hit (window-at geo row col))
  (cond
    [(not hit) (values #f #f 'nothing)]
    [else
     (define rect (leaf-geometry frm hit))
     (define buf (and rect (leaf-buffer hit)))
     (if (and rect buf)
         (let* ([tx (buffer-text buf)]
                [gb (text-gap tx)]
                [win-row (- row (rect-top rect))]
                [win-col (- col (rect-left rect))]
                [start-pos (text-marker-pos tx (leaf-start hit))]
                [max-cols (rect-cols rect)]
                [left-col (leaf-hscroll hit)]
                [wrap-mode (if (truncate-lines? buf) 'none 'char)]
                [vlines (visual-line-lines gb start-pos (add1 win-row) max-cols
                                            #:wrap-mode wrap-mode #:left-col left-col)])
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
                 (values target-pos hit 'text))))
         (values #f #f 'nothing))]))
