#lang racket

(require "../kernel/vbuffer/vbuffer.rkt")

;; display/minibuf.rkt — Minibuffer bottom-line state
;;
;; Adapted from protocol/bottom-input.rkt.
;; Stores echo text, doc lines, or input prompt for the bottom line.

(provide
 ;; state
 bottom-line-state? bottom-line-state-mode
 bottom-line-state-echo-text bottom-line-state-doc-lines
 bottom-line-state-prompt bottom-line-state-input bottom-line-state-cursor

 ;; global
 current-bottom-line
 set-bottom-line-echo! set-bottom-line-doc!
 set-bottom-line-input!

 ;; helpers
 bottom-line-render! bottom-line-doc-rows)

;; ============================================================
;; State
;; ============================================================

(struct bottom-line-state
  ([mode #:mutable]       ; 'idle | 'echo | 'doc | 'input
   [echo-text #:mutable]  ; string | #f
   [doc-lines #:mutable]  ; (listof string) | #f
   [prompt #:mutable]     ; string
   [input #:mutable]      ; string
   [cursor #:mutable])    ; integer
  #:transparent)

(define current-bottom-line
  (make-parameter (bottom-line-state 'idle #f #f "" "" 0)))

;; ============================================================
;; Setters
;; ============================================================

(define (set-bottom-line-echo! text)
  (define bl (current-bottom-line))
  (set-bottom-line-state-mode! bl 'echo)
  (set-bottom-line-state-echo-text! bl text))

(define (set-bottom-line-doc! lines)
  (define bl (current-bottom-line))
  (set-bottom-line-state-mode! bl 'doc)
  (set-bottom-line-state-doc-lines! bl lines))

(define (set-bottom-line-input! prompt input cursor)
  (define bl (current-bottom-line))
  (set-bottom-line-state-mode! bl 'input)
  (set-bottom-line-state-prompt! bl prompt)
  (set-bottom-line-state-input! bl input)
  (set-bottom-line-state-cursor! bl cursor))

;; ============================================================
;; Render — fills vbuffer rows for bottom-line display
;; ============================================================

(define (bottom-line-render! vb row cols)
  (define bl (current-bottom-line))
  (case (bottom-line-state-mode bl)
    [(idle) (values #f 0)]
    [(echo)
     (let ([text (bottom-line-state-echo-text bl)])
       (when text
         (define padded (string-append text (make-string cols #\space)))
         (vbuffer-put-string! vb row 0 (substring padded 0 cols)))
       (values #f 1))]
    [(doc)
     (define lines (bottom-line-state-doc-lines bl))
     (if lines
         (let ([n (min (length lines) 8)])
           (for ([i (in-range n)])
             (define line (list-ref lines i))
             (define padded (string-append line (make-string cols #\space)))
             (vbuffer-put-string! vb (+ row i) 0 (substring padded 0 cols)))
           (values #f n))
         (values #f 1))]
    [(input)
     (define prompt (bottom-line-state-prompt bl))
     (define input  (bottom-line-state-input bl))
     (define cur    (bottom-line-state-cursor bl))
     (define full   (string-append prompt input))
     (define padded (string-append full (make-string cols #\space)))
     (vbuffer-put-string! vb row 0 (substring padded 0 cols))
     (values (+ (string-length prompt) cur) 1)]
    [else (values #f 0)]))

(define (bottom-line-doc-rows)
  (define bl (current-bottom-line))
  (if (eq? (bottom-line-state-mode bl) 'doc)
      (let ([lines (bottom-line-state-doc-lines bl)])
        (if lines (min (length lines) 8) 1))
      1))
