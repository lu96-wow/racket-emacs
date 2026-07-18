#lang racket

;; input/debug/parse-debug.rkt — Debug-wrapped read-key with trace logging
;;
;; ============================================================================
;; Wraps read-key to log every raw byte and the resulting key event.
;; Accumulates entries in-order; flush to file on exit.
;; ============================================================================

(require "../parse.rkt"
         "../key.rkt")

(provide
 read-key-debug     ;; → key-event — wrapped read-key with logging
 parse-debug-flush!  ;; → void — write accumulated trace to file
 parse-debug-reset!) ;; → void — clear trace buffer

;; ============================================================
;; State
;; ============================================================

(define trace-log (box '()))
(define enabled? (make-parameter #t))

;; ============================================================
;; key→s-expression
;; ============================================================

(define (key->string ke)
  (cond
    [(key-char? ke)
     (define ch (key-char-ch ke))
     (define cp (char->integer ch))
     (if (< cp 128)
         (format "(key-char #\\~a)" ch)
         (format "(key-char U+~x)" cp))]
    [(key-ctrl? ke)
     (format "(key-ctrl C-~a)" (key-ctrl-ch ke))]
    [(key-sym? ke)
     (format "(key-sym ~a)" (key-sym-name ke))]
    [(key-mouse? ke)
     (format "(key-mouse ~a (~a ~a) ~a mods=~a)"
             (key-mouse-button ke)
             (key-mouse-x ke) (key-mouse-y ke)
             (key-mouse-action ke)
             (key-mouse-mods ke))]
    [else (format "(unknown ~a)" ke)]))

;; ============================================================
;; read-key-debug — wrapped reader
;; ============================================================

(define (read-key-debug)
  (define ke (read-key))
  (when (enabled?)
    (define entry (key->string ke))
    (set-box! trace-log (cons entry (unbox trace-log))))
  ke)

;; ============================================================
;; Flush / Reset
;; ============================================================

(define (parse-debug-flush! [port (current-output-port)])
  (define entries (reverse (unbox trace-log)))
  (when (and (enabled?) (pair? entries))
    (fprintf port ";; ── input trace (~a events) ──\n" (length entries))
    (for ([e (in-list entries)])
      (fprintf port ";;   ~a\n" e))
    (fprintf port ";; ── end input trace ──\n"))
  (parse-debug-reset!))

(define (parse-debug-reset!)
  (set-box! trace-log '()))
