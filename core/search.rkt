#lang racket

;; core/search.rkt — Pure buffer search (forward/backward)
;;
;; No display, no IO, no key handling.

(require "../kernel/gap.rkt")

(provide search-fwd search-bwd)

;; ============================================================
;; Search
;; ============================================================

(define (string-index str pattern)
  (define plen (string-length pattern))
  (define slen (string-length str))
  (let loop ([i 0])
    (if (> (+ i plen) slen) #f
        (if (string=? (substring str i (+ i plen)) pattern)
            i
            (loop (add1 i))))))

(define (search-fwd gb pattern byte-pos)
  (define s (gap-substring gb byte-pos (gap-byte-length gb)))
  (define m (string-index s pattern))
  (if m (values (+ byte-pos m) (+ byte-pos m (string-length pattern)))
      (values #f #f)))

(define (search-bwd gb pattern byte-pos)
  (define s (gap-substring gb 0 byte-pos))
  (define plen (string-length pattern))
  (define slen (string-length s))
  (let loop ([i (- slen plen)])
    (if (< i 0) (values #f #f)
        (if (and (>= i 0) (string=? (substring s i (+ i plen)) pattern))
            (values i (+ i plen))
            (loop (sub1 i))))))
