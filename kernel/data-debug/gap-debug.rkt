#lang racket

;; kernel/data-debug/gap-debug.rkt — Gap buffer debug S-expressions

(require "../data/gap.rkt"
         "../data/utf8.rkt")

(provide
 gap-debug-summary    ;; gb → "(gap (len N) (pos S E) (cap C))"
 gap-debug-head       ;; gb [n] → "(gap-head N \"text...\")"
 gap-debug-around)    ;; gb pos [r] → "(gap-around P R \"...[...]...\")"

(define (gap-debug-summary gb)
  (format "(gap (len ~a) (pos ~a ~a) (cap ~a))"
          (gap-length gb)
          (gap-buffer-gap-start gb)
          (gap-buffer-gap-end gb)
          (bytes-length (gap-buffer-bytes gb))))

(define (gap-debug-head gb [n 40])
  (define len (min n (gap-length gb)))
  (define bs  (gap-buffer-bytes gb))
  (define gs  (gap-buffer-gap-start gb))
  (define ge  (gap-buffer-gap-end gb))
  (define chars
    (for/list ([i (in-range len)])
      (define phys (physical-index gb i))
      (cond [(and (>= phys gs) (< phys ge)) #\·]
            [else
             (define b (bytes-ref bs phys))
             (if (or (< b 32) (= b 127)) #\. (integer->char b))])))
  (format "(gap-head ~a \"~a\")" len (list->string chars)))

(define (gap-debug-around gb pos [radius 10])
  (define len  (gap-length gb))
  (define from (max 0 (- pos radius)))
  (define to   (min len (+ pos radius)))
  (define chars
    (for/list ([i (in-range from to)])
      (let* ([b (gap-byte-ref gb i)]
             [ch (if (or (< b 32) (= b 127)) #\. (integer->char b))])
        (if (= i pos) (string-append "[" (string ch) "]") (string ch)))))
  (format "(gap-around ~a ~a \"~a\")" pos radius (string-join chars "")))
