#lang racket

;; core/bottom-input.rkt — Bottom-line state machine & input editing
;;
;; Pure logic: zero display or IO dependencies.
;; Manages the bottom row's mode ('idle | 'echo | 'input), editing
;; operations, and history navigation.
;;
;; Rendering is in display/bottom-line.rkt — it reads this state
;; and writes to a vbuffer.

(provide
 (struct-out bottom-line-state)
 current-bottom-line
 bottom-line-mode

 ;; echo
 bottom-line-set-echo! bottom-line-clear-echo!

 ;; doc (multi-line documentation)
 bottom-line-set-doc! bottom-line-clear-doc!
 bottom-line-doc-lines

 ;; input lifecycle
 bottom-line-activate-input! bottom-line-deactivate-input!
 bottom-line-get-input bottom-line-set-input! bottom-line-get-prompt

 ;; input editing
 bottom-line-insert! bottom-line-delete-backward! bottom-line-delete-forward!
 bottom-line-kill-line!
 bottom-line-move-beginning! bottom-line-move-end!
 bottom-line-move-char!

 ;; history
 bottom-line-set-history! bottom-line-history-prev! bottom-line-history-next!)

;; ============================================================
;; State
;; ============================================================

(struct bottom-line-state
  ([mode #:mutable]       ; 'idle | 'echo | 'input | 'doc
   [echo-text #:mutable]  ; string | #f
   [doc-lines #:mutable]  ; (listof string) | #f — for 'doc mode
   [prompt #:mutable]     ; string | #f
   [input #:mutable]      ; string | #f
   [cursor #:mutable]     ; exact-nonnegative-integer? — position within input
   [history #:mutable]    ; (listof string)
   [history-pos #:mutable] ; integer — -1 = not navigating
   )
  #:transparent)

(define current-bottom-line
  (make-parameter (bottom-line-state 'idle #f #f #f #f 0 '() -1)))

(define (bottom-line-mode)
  (bottom-line-state-mode (current-bottom-line)))

;; ============================================================
;; Echo
;; ============================================================

(define (bottom-line-set-echo! text)
  (define bl (current-bottom-line))
  (set-bottom-line-state-mode! bl 'echo)
  (set-bottom-line-state-echo-text! bl text))

(define (bottom-line-clear-echo!)
  (define bl (current-bottom-line))
  (set-bottom-line-state-mode! bl 'idle)
  (set-bottom-line-state-echo-text! bl #f))

;; ============================================================
;; Doc — multi-line documentation in bottom area
;; ============================================================

(define (bottom-line-set-doc! lines)
  (define bl (current-bottom-line))
  (set-bottom-line-state-mode! bl 'doc)
  (set-bottom-line-state-doc-lines! bl lines))

(define (bottom-line-clear-doc!)
  (define bl (current-bottom-line))
  (unless (eq? (bottom-line-state-mode bl) 'input)
    (set-bottom-line-state-mode! bl 'idle))
  (set-bottom-line-state-doc-lines! bl #f))

(define (bottom-line-doc-lines)
  (bottom-line-state-doc-lines (current-bottom-line)))

;; ============================================================
;; Input lifecycle
;; ============================================================

(define (bottom-line-activate-input! prompt
                                     #:initial [initial ""]
                                     #:history [hist '()])
  (define bl (current-bottom-line))
  (set-bottom-line-state-mode! bl 'input)
  (set-bottom-line-state-prompt! bl prompt)
  (set-bottom-line-state-input! bl initial)
  (set-bottom-line-state-cursor! bl (string-length initial))
  (set-bottom-line-state-history! bl hist)
  (set-bottom-line-state-history-pos! bl -1))

(define (bottom-line-deactivate-input!)
  (define bl (current-bottom-line))
  (define input (bottom-line-state-input bl))
  (set-bottom-line-state-mode! bl 'idle)
  (set-bottom-line-state-prompt! bl #f)
  (set-bottom-line-state-input! bl #f)
  (set-bottom-line-state-cursor! bl 0)
  (set-bottom-line-state-history! bl '())
  (set-bottom-line-state-history-pos! bl -1)
  input)

(define (bottom-line-get-input)
  (bottom-line-state-input (current-bottom-line)))

(define (bottom-line-set-input! text)
  (define bl (current-bottom-line))
  (set-bottom-line-state-input! bl text)
  (set-bottom-line-state-cursor! bl (string-length text)))

(define (bottom-line-get-prompt)
  (bottom-line-state-prompt (current-bottom-line)))

;; ============================================================
;; Input editing
;; ============================================================

(define (bottom-line-insert! str)
  (define bl (current-bottom-line))
  (define in (bottom-line-state-input bl))
  (define cur (bottom-line-state-cursor bl))
  (set-bottom-line-state-input! bl
    (string-append (substring in 0 cur) str (substring in cur)))
  (set-bottom-line-state-cursor! bl (+ cur (string-length str))))

(define (bottom-line-delete-backward!)
  (define bl (current-bottom-line))
  (define in (bottom-line-state-input bl))
  (define cur (bottom-line-state-cursor bl))
  (when (> cur 0)
    (set-bottom-line-state-input! bl
      (string-append (substring in 0 (sub1 cur)) (substring in cur)))
    (set-bottom-line-state-cursor! bl (sub1 cur))))

(define (bottom-line-delete-forward!)
  (define bl (current-bottom-line))
  (define in (bottom-line-state-input bl))
  (define cur (bottom-line-state-cursor bl))
  (when (< cur (string-length in))
    (set-bottom-line-state-input! bl
      (string-append (substring in 0 cur) (substring in (add1 cur))))))

(define (bottom-line-kill-line!)
  (define bl (current-bottom-line))
  (define in (bottom-line-state-input bl))
  (define cur (bottom-line-state-cursor bl))
  (set-bottom-line-state-input! bl (substring in 0 cur)))

(define (bottom-line-move-beginning!)
  (set-bottom-line-state-cursor! (current-bottom-line) 0))

(define (bottom-line-move-end!)
  (define bl (current-bottom-line))
  (set-bottom-line-state-cursor! bl (string-length (bottom-line-state-input bl))))

(define (bottom-line-move-char! delta)
  (define bl (current-bottom-line))
  (define in (bottom-line-state-input bl))
  (define cur (bottom-line-state-cursor bl))
  (set-bottom-line-state-cursor! bl
    (max 0 (min (string-length in) (+ cur delta)))))

;; ============================================================
;; History
;; ============================================================

(define (bottom-line-set-history! hist)
  (define bl (current-bottom-line))
  (set-bottom-line-state-history! bl hist)
  (set-bottom-line-state-history-pos! bl -1))

(define (bottom-line-history-prev!)
  (define bl (current-bottom-line))
  (define hist (bottom-line-state-history bl))
  (define pos (bottom-line-state-history-pos bl))
  (when (pair? hist)
    (define new-pos (min (add1 pos) (sub1 (length hist))))
    (set-bottom-line-state-history-pos! bl new-pos)
    (define val (list-ref hist new-pos))
    (set-bottom-line-state-input! bl val)
    (set-bottom-line-state-cursor! bl (string-length val))))

(define (bottom-line-history-next!)
  (define bl (current-bottom-line))
  (define pos (bottom-line-state-history-pos bl))
  (when (>= pos 0)
    (define new-pos (sub1 pos))
    (set-bottom-line-state-history-pos! bl new-pos)
    (define val (if (>= new-pos 0)
                    (list-ref (bottom-line-state-history bl) new-pos)
                    ""))
    (set-bottom-line-state-input! bl val)
    (set-bottom-line-state-cursor! bl (string-length val))))
