#lang racket

;; base/kill-ring.rkt — Kill ring (clipboard for killed text)
;;
;; Dependency-free. Stores strings.

(provide
 kill-new kill-append
 kill-ring-yank kill-ring-pop
 kill-ring-empty?
 current-kill)

;; ============================================================
;; Kill ring
;; ============================================================

(define kill-ring (box '()))
(define kill-ring-yank-pointer (box #f)) ; #f or pair whose car is current yank
(define last-command-was-kill (box #f))

(define (kill-new text)
  (set-box! kill-ring (cons text (unbox kill-ring)))
  (set-box! kill-ring-yank-pointer (unbox kill-ring))
  (set-box! last-command-was-kill #t))

(define (kill-append text before?)
  (define ring (unbox kill-ring))
  (when (pair? ring)
    (define combined
      (if before?
          (string-append text (car ring))
          (string-append (car ring) text)))
    (set-box! kill-ring (cons combined (cdr ring)))
    (set-box! kill-ring-yank-pointer (unbox kill-ring))))

(define (kill-ring-yank)
  (define ptr (unbox kill-ring-yank-pointer))
  (and ptr (pair? ptr) (car ptr)))

(define (kill-ring-pop)
  (define ptr (unbox kill-ring-yank-pointer))
  (when (and ptr (pair? (cdr ptr)))
    (set-box! kill-ring-yank-pointer (cdr ptr))
    (car (cdr ptr))))

(define (kill-ring-empty?)
  (null? (unbox kill-ring)))

(define (current-kill)
  (define ring (unbox kill-ring))
  (and (pair? ring) (car ring)))
