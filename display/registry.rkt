#lang racket

;; display/registry.rkt — Buffer registry (get-buffer-create, switch, kill)
;;
;; Adapted from base/registry.rkt for rebuild buffer API.

(require "../kernel/buffer.rkt")

(provide
 buffer-registry? buffer-registry-by-name
 the-buffer-registry
 register-buffer! unregister-buffer!
 get-buffer-create get-buffer
 kill-buffer buffer-live-p
 buffer-list other-buffer
 switch-to-buffer
 current-buffer set-buffer)

;; ============================================================
;; Registry
;; ============================================================

(struct buffer-registry
  ([by-name #:mutable] [by-name-ci #:mutable])
  #:transparent)

(define the-buffer-registry (buffer-registry (make-hash) (make-hash)))

(define (register-buffer! buf)
  (define name (buffer-name buf))
  (define ci (string-downcase name))
  (hash-set! (buffer-registry-by-name the-buffer-registry) name buf)
  (hash-set! (buffer-registry-by-name-ci the-buffer-registry) ci buf))

(define (unregister-buffer! buf)
  (define name (buffer-name buf))
  (hash-remove! (buffer-registry-by-name the-buffer-registry) name)
  (hash-remove! (buffer-registry-by-name-ci the-buffer-registry) (string-downcase name)))

(define (get-buffer-create name #:inhibit-hooks? [inhibit? #f])
  (define ci (string-downcase name))
  (or (hash-ref (buffer-registry-by-name-ci the-buffer-registry) ci #f)
      (let ([buf (make-buffer name)])
        (register-buffer! buf) buf)))

(define (get-buffer name)
  (define ci (string-downcase name))
  (hash-ref (buffer-registry-by-name-ci the-buffer-registry) ci (λ () #f)))

(define (kill-buffer #:buf [b (current-buffer)])
  (unregister-buffer! b)
  (when (eq? b (current-buffer))
    (define other (other-buffer #:exclude b))
    (when other (set-buffer other))))

(define (buffer-live-p b)
  (hash-has-key? (buffer-registry-by-name the-buffer-registry) (buffer-name b)))

(define (buffer-list)
  (hash-values (buffer-registry-by-name the-buffer-registry)))

(define (other-buffer #:visible-ok? [visible-ok? #f] #:exclude [exclude #f])
  (for/or ([b (in-list (buffer-list))])
    (and (not (eq? b exclude)) b)))

(define (switch-to-buffer buf-or-name)
  (define buf (if (buffer? buf-or-name)
                  buf-or-name
                  (get-buffer-create buf-or-name)))
  (set-buffer buf) buf)

;; ============================================================
;; current-buffer — global parameter
;; ============================================================

(define current-buffer (make-parameter #f))

(define (set-buffer buf)
  (current-buffer buf)
  buf)
