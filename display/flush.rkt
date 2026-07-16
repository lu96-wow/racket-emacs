#lang racket

;; display/flush.rkt — vbuffer → terminal output (delta flush)
;;
;; Compares new vbuffer against cached previous frame.
;; Only outputs changed rows.  State machine tracks active face-id
;; and attributes to minimize ANSI escape overhead.
;;
;; This is the ONLY module in display/ with side effects (display to stdout).
;; Dependencies: display/vbuffer, display/face, platform/ansi.

(require "vbuffer.rkt"
         "face.rkt"
         "char-width.rkt"
         "../platform/ansi.rkt")

(provide
 flush-vbuffer!
 flush-vbuffer-delta!)

;; ============================================================
;; flush-vbuffer! — full flush (first frame or after resize)
;; ============================================================

(define (flush-vbuffer! vb)
  (define cells (vbuffer-cells vb))
  (define cols (vbuffer-cols vb))
  (define rows (vbuffer-rows vb))
  (define faces-by-id (face-cache-by-id (current-face-cache)))
  (define out (open-output-string))

  (for ([r (in-range rows)])
    (display (format-cursor-move r 0) out)
    (flush-row-cells! out cols cells (* r cols) faces-by-id)
    (display format-clear-to-eol out))

  (display (get-output-string out)))

;; ============================================================
;; flush-vbuffer-delta! — row-by-row diff against cache
;; ============================================================

(define (flush-vbuffer-delta! new-vb cache-vb)
  (define new-cells (vbuffer-cells new-vb))
  (define cols (vbuffer-cols new-vb))
  (define rows (vbuffer-rows new-vb))
  (define faces-by-id (face-cache-by-id (current-face-cache)))
  (define out (open-output-string))

  (for ([r (in-range rows)])
    (when (or (not cache-vb)
              (not (= cols (vbuffer-cols cache-vb)))
              (not (= rows (vbuffer-rows cache-vb)))
              (vbuffer-row-changed? new-vb cache-vb r))
      (display (format-cursor-move r 0) out)
      (flush-row-cells! out cols new-cells (* r cols) faces-by-id)
      (display format-clear-to-eol out)))

  (display (get-output-string out)))

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

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)

  (parameterize ([color-depth 'truecolor])
    (init-face-cache!)

    (test-case "flush-vbuffer! produces output"
      (let ([vb (make-vbuffer 2 5)])
        (vbuffer-put-string! vb 0 0 "hello")
        (vbuffer-put-string! vb 1 0 "world")
        (define out (with-output-to-string
                      (λ () (flush-vbuffer! vb))))
        ;; Should contain cursor moves and text
        (check-true (string-contains? out "hello"))
        (check-true (string-contains? out "world"))))

    (test-case "flush-vbuffer-delta! skips unchanged rows"
      (let ([vb1 (make-vbuffer 3 5)]
            [vb2 (make-vbuffer 3 5)])
        (vbuffer-put-string! vb1 0 0 "abcde")
        (vbuffer-put-string! vb1 1 0 "fghij")
        ;; vb2 same as vb1 initially
        (vbuffer-put-string! vb2 0 0 "abcde")
        (vbuffer-put-string! vb2 1 0 "fghij")
        ;; Change row 2 in vb2
        (vbuffer-put-string! vb2 2 0 "ZZZZZ")
        (define out (with-output-to-string
                      (λ () (flush-vbuffer-delta! vb2 vb1))))
        ;; Should only contain row 2 (index 2)
        (check-true (string-contains? out "ZZZZZ"))
        ;; Should NOT contain row 0 or 1 content
        (check-false (string-contains? out "abcde"))
        (check-false (string-contains? out "fghij"))))

    (test-case "face changes produce ANSI codes"
      (let* ([vb (make-vbuffer 1 10)])
        (define fc (current-face-cache))
        (define-face! 'keyword (make-face-attrs attr-foreground 1))
        (define kid (face-id-for-name 'keyword))
        (vbuffer-put-char! vb 0 0 #\H #:face-id 0)
        (vbuffer-put-char! vb 0 1 #\i #:face-id kid)
        (define out (with-output-to-string
                      (λ () (flush-vbuffer! vb))))
        ;; Should contain ANSI escape for color change
        (check-true (string-contains? out "\e[38"))
        (check-true (string-contains? out "Hi"))))))
