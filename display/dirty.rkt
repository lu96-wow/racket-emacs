#lang racket

;; display/dirty.rkt — Redisplay dirty flag system
;;
;; A single global flag + per-leaf flags.
;; Set by commands, checked by event loop at safe points.
;; Lives in display/ because it bridges command (api) and render (display).

(provide
 redisplay-needed? mark-redisplay-needed! clear-redisplay-needed!
 leaf-dirty? mark-leaf-dirty! clear-leaf-dirty!)

;; Global redisplay flag — set by commands that change buffer content
(define redisplay-needed-box (box #f))

(define (redisplay-needed?) (unbox redisplay-needed-box))
(define (mark-redisplay-needed!) (set-box! redisplay-needed-box #t))
(define (clear-redisplay-needed!) (set-box! redisplay-needed-box #f))

;; Per-leaf dirty flags
(define leaf-dirty-table (make-hasheq))

(define (leaf-dirty? lf)
  (hash-ref leaf-dirty-table lf #t))

(define (mark-leaf-dirty! lf)
  (hash-set! leaf-dirty-table lf #t))

(define (clear-leaf-dirty! lf)
  (hash-set! leaf-dirty-table lf #f))
