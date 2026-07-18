#lang racket

;; kernel/kill-ring.rkt — Kill ring (clipboard for killed text)
;;
;; ============================================================================
;; Zero dependencies.  Stores strings in a mutable ring (cons chain).
;; Bounded by `kill-ring-max` (default 60) — excess entries discarded at tail.
;; ============================================================================
;;
;;   kill-new       : string? → void    — push text as new entry
;;   kill-append    : string? boolean? → void — merge into head entry
;;   kill-ring-yank : → (or/c string? #f) — current entry
;;   kill-ring-pop  : → (or/c string? #f) — advance to next entry
;;   kill-ring-empty? : → boolean?
;;   current-kill   : → (or/c string? #f)
;;   kill-ring-max  : parameter? — max entries (default 60)
;;
;; ============================================================================

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
(define kill-ring     (box '()))     ;; cons chain, newest at head
(define kill-ring-len (box 0))       ;; current length
(define yank-ptr      (box #f))      ;; current position for yank-pop
(define last-was-kill (box #f))      ;; was the last command a kill?

;; ============================================================
;; truncate! — cut tail when over max
;; ============================================================

(define (truncate!)
  (define max-len (kill-ring-max))
  (when (> (unbox kill-ring-len) max-len)
    (set-box! kill-ring (take (unbox kill-ring) max-len))
    (set-box! kill-ring-len max-len)))

;; ============================================================
;; Operations
;; ============================================================

(define (kill-new text)
  ;; Push `text` as a new kill-ring entry.
  ;; Resets yank-ptr to the new head.
  (unless (string? text)
    (raise-argument-error 'kill-new "string?" text))
  (set-box! kill-ring (cons text (unbox kill-ring)))
  (set-box! kill-ring-len (add1 (unbox kill-ring-len)))
  (truncate!)
  (set-box! yank-ptr (unbox kill-ring))
  (set-box! last-was-kill #t))

(define (kill-append text before?)
  ;; Merge `text` into the existing head entry.
  ;; `before?` = #t → prepend, #f → append.
  ;; Does not create a new entry → length unchanged.
  (unless (string? text)
    (raise-argument-error 'kill-append "string?" text))
  (define ring (unbox kill-ring))
  (when (pair? ring)
    (define combined
      (if before?
          (string-append text (car ring))
          (string-append (car ring) text)))
    (set-box! kill-ring (cons combined (cdr ring)))
    (set-box! yank-ptr (unbox kill-ring))))

(define (kill-ring-yank)
  ;; Return the current yank-pointer's text, or #f if empty.
  (define ptr (unbox yank-ptr))
  (and ptr (pair? ptr) (car ptr)))

(define (kill-ring-pop)
  ;; Advance yank-ptr to the previous (older) entry.
  ;; Returns the new current text, or #f if at end.
  (define ptr (unbox yank-ptr))
  (when (and ptr (pair? (cdr ptr)))
    (set-box! yank-ptr (cdr ptr))
    (car (cdr ptr))))

(define (kill-ring-empty?)
  (null? (unbox kill-ring)))

(define (current-kill)
  ;; Most recently killed text (head of ring).
  (define ring (unbox kill-ring))
  (and (pair? ring) (car ring)))
