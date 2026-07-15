#lang racket

;; display/bottom-line.rkt — Bottom-line rendering
;;
;; Reads state from core/bottom-input.rkt and renders it into a vbuffer row.
;; Supports 'doc mode: temporarily expands to multiple rows for documentation.
;; Pure rendering: no state mutation, no IO — just state → vbuffer cells.

(require "../kernel/bottom-input.rkt"
         "../kernel/window.rkt"
         "vbuffer.rkt")

(provide bottom-line-render! bottom-line-doc-rows)

;; ============================================================
;; Render
;; ============================================================

(define (bottom-line-render! vb row cols)
  ;; Renders current bottom-line state into vb starting at row `row`.
  ;; The caller is responsible for having enough rows in vb.
  ;; Returns: (values cursor-col-or-#f rows-consumed)
  (define bl (current-bottom-line))
  (case (bottom-line-state-mode bl)
    [(idle) (values #f 0)]
    [(echo)
     (let ([text (bottom-line-state-echo-text bl)])
       (when text
         (let ([padded (string-append text (make-string cols #\space))])
           (vbuffer-put-string! vb row 0 (substring padded 0 cols))))
       (values #f 1))]
    [(doc)
     (let ([lines (bottom-line-state-doc-lines bl)])
       (if lines
           (let ([n (min (length lines) 8)])  ;; max 8 lines
             (for ([i (in-range n)])
               (let* ([line (list-ref lines i)]
                      [padded (string-append line (make-string cols #\space))])
                 (vbuffer-put-string! vb (+ row i) 0
                   (substring padded 0 cols))))
             (values #f n))
           (values #f 1)))]
    [(input)
     (let* ([prompt (bottom-line-state-prompt bl)]
            [input (bottom-line-state-input bl)]
            [cur (bottom-line-state-cursor bl)]
            [full (string-append prompt input)]
            [padded (string-append full (make-string cols #\space))])
       (vbuffer-put-string! vb row 0 (substring padded 0 cols))
       (values (+ (string-length prompt) cur) 1))]
    [else (values #f 0)]))

;; How many rows does the bottom-line need right now?
(define (bottom-line-doc-rows)
  (define bl (current-bottom-line))
  (if (eq? (bottom-line-state-mode bl) 'doc)
      (let ([lines (bottom-line-state-doc-lines bl)])
        (if lines (min (length lines) 8) 1))
      1))
