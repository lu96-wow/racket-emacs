#lang racket

;; user/completion-ui.rkt — Minibuffer completion (MVP)
;;
;; completing-read  — prompt + source → chosen string or #f
;; minibuffer-complete — TAB handler (common prefix + echo)

(require "../kernel/buffer.rkt"
         "../kernel/completion.rkt"
         "../base/completion-algo.rkt"
         "../kernel/bottom-input.rkt"
         "../core/minibuffer.rkt"
         "../base/registry.rkt"
         "minibuffer-loop.rkt")

(provide
 completing-read
 current-completion-source
 minibuffer-complete)

;; Default styles active for minibuffer completion.
(completion-styles (list prefix-completion-style))

;; ============================================================
;; Per-completion state
;; ============================================================

(define current-completion-source (make-parameter #f))

;; ============================================================
;; completing-read
;; ============================================================

(define (completing-read prompt source
                         #:require-match? [require-match? #f]
                         #:initial-input [initial ""]
                         #:history [hist '()])
  (parameterize ([current-completion-source source])
    (read-from-minibuffer! prompt
                           #:keymap minibuffer-local-map
                           #:initial initial)))

;; ============================================================
;; minibuffer-complete — TAB action
;; ============================================================

(define (minibuffer-complete)
  (define source (current-completion-source))
  (unless source
    (bottom-line-set-echo! "[no completion source]"))
  (when source
    (define input (bottom-line-get-input))
    (define candidates (completion-candidates source input))

    (cond
      [(null? candidates)
       (bottom-line-set-echo! "[no match]")]
      [(null? (cdr candidates))
       ;; Unique match: fill it in
       (bottom-line-set-input! (car candidates))]
      [else
       ;; Multiple matches: longest common prefix first
       (define lcp (longest-common-prefix candidates))
       (if (> (string-length lcp) (string-length input))
           (bottom-line-set-input! lcp)
           (bottom-line-set-echo!
            (format "[~a matches] ~a"
                    (length candidates)
                    (string-join (take candidates 5) " "))))])))

;; ============================================================
;; Helpers
;; ============================================================

(define (longest-common-prefix strs)
  (if (or (null? strs) (null? (cdr strs)))
      (if (null? strs) "" (car strs))
      (let ([s0 (car strs)])
        (let loop ([i 0])
          (if (>= i (string-length s0))
              s0
              (let ([ch (string-ref s0 i)])
                (if (for/and ([s (in-list (cdr strs))])
                      (and (< i (string-length s))
                           (char=? (string-ref s i) ch)))
                    (loop (add1 i))
                    (substring s0 0 i))))))))

(define (take lst n)
  (if (or (null? lst) (zero? n)) '()
      (cons (car lst) (take (cdr lst) (sub1 n)))))
