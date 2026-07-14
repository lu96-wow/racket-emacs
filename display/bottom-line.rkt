#lang racket

;; display/bottom-line.rkt — Bottom-line rendering
;;
;; Reads state from core/bottom-input.rkt and renders it into a vbuffer row.
;; Pure rendering: no state mutation, no IO — just state → vbuffer cells.

(require "../kernel/bottom-input.rkt"
         "vbuffer.rkt")

(provide bottom-line-render!)

;; ============================================================
;; Render
;; ============================================================

(define (bottom-line-render! vb row cols)
  ;; Renders current bottom-line state into vb at row `row`.
  ;; Returns: (or/c #f exact-nonnegative-integer?) — cursor column, or #f.
  (define bl (current-bottom-line))
  (case (bottom-line-state-mode bl)
    [(idle) #f]
    [(echo)
     (define text (bottom-line-state-echo-text bl))
     (when text
       (define padded (string-append text (make-string cols #\space)))
       (vbuffer-put-string! vb row 0 (substring padded 0 cols)))
     #f]
    [(input)
     (define prompt (bottom-line-state-prompt bl))
     (define input (bottom-line-state-input bl))
     (define cur (bottom-line-state-cursor bl))
     (define full (string-append prompt input))
     (define padded (string-append full (make-string cols #\space)))
     (vbuffer-put-string! vb row 0 (substring padded 0 cols))
     (+ (string-length prompt) cur)]
    [else #f]))
