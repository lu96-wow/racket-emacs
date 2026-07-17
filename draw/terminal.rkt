#lang racket

;; platform/terminal.rkt — vbuffer → ANSI terminal output (delta flush)
;;
;; Compares new vbuffer against cached previous frame.
;; Only outputs changed rows.  State machine tracks active face-id
;; and attributes to minimize ANSI escape overhead.
;;
;; This is the only module in the pipeline with terminal side effects.
;; Dependencies: display/vbuffer, display/face, platform/ansi.

(require "../display/vbuffer.rkt"
         "../display/face.rkt"
         "../kernel/data/char-width.rkt"
         "../platform/ansi.rkt")

(provide
 terminal-flush!
 terminal-flush-delta!)

;; ============================================================
;; terminal-flush! — full flush (first frame or after resize)
;; ============================================================

(define (terminal-flush! vb face-cache)

  (define cells (vbuffer-cells vb))
  (define cols (vbuffer-cols vb))
  (define rows (vbuffer-rows vb))
  (define faces-by-id (face-cache-by-id face-cache))
  (define out (open-output-string))

  (for ([r (in-range rows)])
    (display (format-cursor-move r 0) out)
    (flush-row-cells! out cols cells (* r cols) faces-by-id)
    (display format-clear-to-eol out))

  (get-output-string out))

;; ============================================================
;; terminal-flush-delta! — row-by-row diff against cache
;; ============================================================

(define (terminal-flush-delta! new-vb cache-vb face-cache)
  (define new-cells (vbuffer-cells new-vb))
  (define cols (vbuffer-cols new-vb))
  (define rows (vbuffer-rows new-vb))
  (define faces-by-id (face-cache-by-id face-cache))
  (define out (open-output-string))

  (for ([r (in-range rows)])
    (when (or (not cache-vb)
              (not (= cols (vbuffer-cols cache-vb)))
              (not (= rows (vbuffer-rows cache-vb)))
              (vbuffer-row-changed? new-vb cache-vb r))
      (display (format-cursor-move r 0) out)
      (flush-row-cells! out cols new-cells (* r cols) faces-by-id)
      (display format-clear-to-eol out)))

  (get-output-string out))

;; ============================================================
;; flush-row-cells! — ANSI state machine for one row
;; ============================================================

(define (flush-row-cells! out cols cells row-start faces-by-id)
  (define active-face-id  0)
  (define active-attrs #f)

  (let loop ([c 0] [skip? #f])
    (when (< c cols)
      (define cl (vector-ref cells (+ row-start c)))
      (if skip?
          (loop (add1 c) #f)
          (let* ([ch          (cell-ch cl)]
                 [new-face-id (cell-face-id cl)]
                 [new-attrs   (cell-attrs cl)]
                 [new-syms    (cond [(not new-attrs) '()]
                                    [(symbol? new-attrs) (list new-attrs)]
                                    [(list? new-attrs) new-attrs]
                                    [else '()])]
                 [old-syms (if active-attrs active-attrs '())]
                 [face-changed?  (not (= new-face-id active-face-id))]
                 [attrs-changed? (not (equal? new-syms old-syms))])
            (cond
              [face-changed?
               (display format-reset out)
               (define rf (and (< new-face-id (vector-length faces-by-id))
                               (vector-ref faces-by-id new-face-id)))
               (define face-ansi (and rf (realized-face-ansi-bytes rf)))
               (when (and face-ansi (positive? (bytes-length face-ansi)))
                 (display face-ansi out))
               (set! active-face-id new-face-id)
               (set! active-attrs (if (null? new-syms) #f new-syms))
               (unless (null? new-syms)
                 (define bs (attrs->bytes new-attrs))
                 (when bs (display bs out)))]
              [attrs-changed?
               (set! active-attrs (if (null? new-syms) #f new-syms))
               (if (null? new-syms)
                   (display format-reset out)
                   (let ([bs (attrs->bytes new-attrs)])
                     (when bs (display bs out))))]
              [else (void)])
            (display ch out)
            (loop (add1 c) (= (char-display-width ch) 2))))))

  ;; Reset at end of row
  (when (or active-attrs (not (= active-face-id 0)))
    (display format-reset out)))

;; ============================================================
;; attr helpers
;; ============================================================

(define attr->format
  (hasheq 'reverse   format-reverse
          'bold      format-bold
          'dim       format-dim
          'italic    format-italic
          'underline format-underline
          'blink     format-blink))

(define (attrs->bytes attrs)
  (define syms (cond [(not attrs) '()]
                     [(symbol? attrs) (list attrs)]
                     [(list? attrs) attrs]
                     [else '()]))
  (define strs
    (for/list ([s (in-list syms)] #:when (hash-has-key? attr->format s))
      (hash-ref attr->format s)))
  (if (null? strs) #f (string->bytes/utf-8 (string-join strs ""))))
