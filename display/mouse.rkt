#lang racket

;; display/mouse.rkt — Mouse click → buffer position
;;
;; Dependencies: kernel/buffer, display/layout, display/window

(require "../kernel/buffer.rkt"
         "../kernel/text.rkt"
         "../kernel/gap/query.rkt"
         "char-width.rkt"
         "layout.rkt"
         "window.rkt")

(provide screen-coord->buffer-pos)

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
