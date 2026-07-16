#lang racket

;; display/flush.rkt — vbuffer → terminal output
;;
;; Delta flush: diff new vbuffer against cache, output only changed rows.
;; Face/attr state machine minimizes ANSI escape overhead.
;;
;; Dependencies: kernel/vbuffer, display/face, platform/ansi

(require "../kernel/vbuffer/vbuffer.rkt"
         "../platform/ansi.rkt"
         "char-width.rkt"
         "face.rkt")

(provide
 flush-vbuffer-delta!
 flush-row-cells!
 flush-vbuffer!
 attr->format attrs->bytes)

;; ============================================================
;; flush-vbuffer-delta! — row-by-row diff
;; ============================================================

(define (flush-vbuffer-delta! new-vb cache)
  (define new-cells (vbuffer-cells new-vb))
  (define cols (vbuffer-cols new-vb))
  (define rows (vbuffer-rows new-vb))
  (define old-cells (and cache
                         (= cols (vbuffer-cols cache))
                         (= rows (vbuffer-rows cache))
                         (vbuffer-cells cache)))
  (define faces-by-id (face-cache-by-id (current-face-cache)))
  (define out (open-output-string))
  (for ([r (in-range rows)])
    (define row-start (* r cols))
    (define row-changed?
      (or (not old-cells)
          (for/or ([c (in-range cols)])
            (define i (+ row-start c))
            (define nc (vector-ref new-cells i))
            (define oc (vector-ref old-cells i))
            (not (and (char=? (cell-ch nc) (cell-ch oc))
                      (= (cell-face-id nc) (cell-face-id oc))
                      (equal? (cell-attrs nc) (cell-attrs oc)))))))
    (when row-changed?
      (display (format-cursor-move r 0) out)
      (flush-row-cells! out cols new-cells row-start faces-by-id)
      (display "\e[K" out)))
  (display (get-output-string out)))

;; ============================================================
;; flush-row-cells! — output one row with face/attr state machine
;; ============================================================

(define (flush-row-cells! out cols cells row-start faces-by-id)
  (define active-attrs #f)
  (define active-face-id 0)
  (define skip-next? #f)
  (for ([c (in-range cols)])
    (define cl (vector-ref cells (+ row-start c)))
    (define ch (cell-ch cl))
    (cond
      [skip-next? (set! skip-next? #f)]
      [else
       (define new-attrs (cell-attrs cl))
       (define new-syms (cond [(not new-attrs) '()]
                              [(symbol? new-attrs) (list new-attrs)]
                              [(list? new-attrs) new-attrs]
                              [else '()]))
       (define old-syms (if active-attrs active-attrs '()))
       (define face-changed? (not (= (cell-face-id cl) active-face-id)))
       (define attrs-changed? (not (equal? new-syms old-syms)))
       (cond
         [face-changed?
          (display format-reset out)
          (define rf (vector-ref faces-by-id (cell-face-id cl)))
          (define face-ansi (realized-face-ansi-bytes rf))
          (when (positive? (bytes-length face-ansi)) (display face-ansi out))
          (set! active-face-id (cell-face-id cl))
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
       (when (= (char-display-width ch) 2) (set! skip-next? #t))]))
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
  (define strs (for/list ([s (in-list syms)]
                          #:when (hash-has-key? attr->format s))
                 (hash-ref attr->format s)))
  (if (null? strs) #f (string->bytes/utf-8 (string-join strs ""))))

;; ============================================================
;; flush-vbuffer! — full output (for testing)
;; ============================================================

(define (flush-vbuffer! vb)
  (flush-vbuffer-delta! vb #f))
