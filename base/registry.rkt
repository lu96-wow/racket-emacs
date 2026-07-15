#lang racket

;; base/registry.rkt — Buffer registry (get-buffer-create, switch, kill)

(require "../kernel/buffer.rkt"
         "../kernel/syntax.rkt"
         "../kernel/font-lock.rkt"
         "../kernel/keymap.rkt")

(provide
 buffer-registry? buffer-registry-by-name
 buffer-registry-on-kill set-buffer-registry-on-kill!
 the-buffer-registry
 register-buffer! unregister-buffer!
 get-buffer-create get-buffer
 kill-buffer buffer-live-p
 buffer-list other-buffer
 rename-buffer switch-to-buffer
 kill-buffer-hook kill-buffer-query-functions)

(struct buffer-registry
  ([by-name #:mutable] [by-name-ci #:mutable] [on-kill #:mutable]) #:transparent)

(define the-buffer-registry (buffer-registry (make-hash) (make-hash) '()))

(define (register-buffer! buf)
  (define name (buffer-name buf)) (define ci (string-downcase name))
  (hash-set! (buffer-registry-by-name the-buffer-registry) name buf)
  (hash-set! (buffer-registry-by-name-ci the-buffer-registry) ci buf))

(define (unregister-buffer! buf)
  (define name (buffer-name buf))
  (hash-remove! (buffer-registry-by-name the-buffer-registry) name)
  (hash-remove! (buffer-registry-by-name-ci the-buffer-registry) (string-downcase name)))

(define (get-buffer-create name #:inhibit-hooks? [inhibit? #f])
  (define ci (string-downcase name))
  (or (hash-ref (buffer-registry-by-name-ci the-buffer-registry) ci #f)
      (let ([buf (make-buffer name)]) (register-buffer! buf) buf)))

(define (get-buffer name)
  (define ci (string-downcase name))
  (hash-ref (buffer-registry-by-name-ci the-buffer-registry) ci (λ () #f)))

(define kill-buffer-hook (make-parameter '()))
(define kill-buffer-query-functions (make-parameter '()))

(define (kill-buffer #:buf [b (current-buffer)])
  (for ([f (in-list (kill-buffer-query-functions))]) (unless (f) (error 'kill-buffer "aborted")))
  (for ([f (in-list (buffer-registry-on-kill the-buffer-registry))]) (f b))
  (for ([f (in-list (kill-buffer-hook))]) (f))
  (unregister-buffer! b)
  (buffer-cleanup! b)
  (syntax-buffer-cleanup! b)
  (font-lock-buffer-cleanup! b)
  (keymap-buffer-cleanup! b)
  (define other (other-buffer #:visible-ok? #t #:exclude b))
  (when (and other (eq? b (current-buffer))) (set-buffer other)))

(define (buffer-live-p b) (hash-has-key? (buffer-registry-by-name the-buffer-registry) (buffer-name b)))
(define (switch-to-buffer buf-or-name) (define buf (get-buffer-create buf-or-name)) (set-buffer buf) buf)
(define (buffer-list) (hash-values (buffer-registry-by-name the-buffer-registry)))

(define (other-buffer #:visible-ok? [visible-ok? #f] #:exclude [exclude #f])
  (for/or ([b (in-list (buffer-list))]) (and (not (eq? b exclude)) b)))

(define (rename-buffer newname)
  (define buf (current-buffer)) (define oldname (buffer-name buf))
  (hash-remove! (buffer-registry-by-name the-buffer-registry) oldname)
  (hash-remove! (buffer-registry-by-name-ci the-buffer-registry) (string-downcase oldname))
  (set-buffer-name! buf newname) (register-buffer! buf))
