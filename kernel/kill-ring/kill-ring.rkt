#lang racket

;; kernel/kill-ring/kill-ring.rkt — Kill ring (clipboard for killed text)
;;
;; Zero dependencies. Stores strings in a mutable ring (cons chain).
;; Bounded by `kill-ring-max` (default 60) — excess entries discarded at tail.

(provide
 kill-new kill-append
 kill-ring-yank kill-ring-pop
 kill-ring-empty?
 current-kill
 kill-ring-max)

;; ============================================================
;; State
;; ============================================================

(define kill-ring-max (make-parameter 60))
(define kill-ring     (box '()))
(define kill-ring-len (box 0))
(define yank-ptr      (box #f))
(define last-was-kill (box #f))

;; ============================================================
;; truncate! — cut tail when over max
;; ============================================================

(define (truncate!)
  (define max (kill-ring-max))
  (when (> (unbox kill-ring-len) max)
    (set-box! kill-ring (take (unbox kill-ring) max))
    (set-box! kill-ring-len max)))

;; ============================================================
;; Operations
;; ============================================================

(define (kill-new text)
  (set-box! kill-ring (cons text (unbox kill-ring)))
  (set-box! kill-ring-len (add1 (unbox kill-ring-len)))
  (truncate!)
  (set-box! yank-ptr (unbox kill-ring))
  (set-box! last-was-kill #t))

(define (kill-append text before?)
  ;; Merge text into the existing head entry.
  ;; Does not create a new entry → length unchanged.
  (define ring (unbox kill-ring))
  (when (pair? ring)
    (define combined
      (if before?
          (string-append text (car ring))
          (string-append (car ring) text)))
    (set-box! kill-ring (cons combined (cdr ring)))
    (set-box! yank-ptr (unbox kill-ring))))

(define (kill-ring-yank)
  (define ptr (unbox yank-ptr))
  (and ptr (pair? ptr) (car ptr)))

(define (kill-ring-pop)
  (define ptr (unbox yank-ptr))
  (when (and ptr (pair? (cdr ptr)))
    (set-box! yank-ptr (cdr ptr))
    (car (cdr ptr))))

(define (kill-ring-empty?)
  (null? (unbox kill-ring)))

(define (current-kill)
  (define ring (unbox kill-ring))
  (and (pair? ring) (car ring)))
