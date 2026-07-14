#lang racket

;; user/mode.rkt — Flat mode setup: register named setup functions directly.
;;
;; No mode struct, no define-mode DSL.
;; Each "mode" is just a (buffer -> void) function that writes to kernel hashes.

(require "../kernel/buffer.rkt"
         "../kernel/keymap.rkt"
         "../kernel/syntax.rkt"
         "font-lock-activate.rkt")

(provide
 setup-buffer-mode!    ; (buffer symbol -> void)  apply named setup
 auto-setup-buffer!    ; (buffer path -> void)    auto-detect by filename
 register-mode-setup!) ; (symbol (buffer->void) (listof string) -> void)

;; Internal: name → (cons setup-fn file-types)
(define mode-setups (make-hash))

(define (register-mode-setup! name setup-fn file-types)
  (hash-set! mode-setups name (cons setup-fn file-types)))

(define (setup-buffer-mode! buf name)
  (define pair (hash-ref mode-setups name (λ () #f)))
  (unless pair (error 'setup-buffer-mode! "unknown mode: ~a" name))
  ((car pair) buf))

(define (auto-setup-buffer! buf path)
  (define ext (path-extension path))
  (for/or ([(name pair) (in-hash mode-setups)])
    (and (member ext (cdr pair))
         (begin ((car pair) buf) name))))

(define (path-extension path)
  (define s (if (path? path) (path->string path) path))
  (define dot-idx
    (let loop ([i (sub1 (string-length s))])
      (cond [(< i 0) #f]
            [(char=? (string-ref s i) #\.) i]
            [else (loop (sub1 i))])))
  (if dot-idx (substring s dot-idx) ""))
