#lang racket

;; kernel/syntax.rkt — Syntax table kernel primitives
;;
;; struct + char-syntax + predicates only.
;; Standard and Lisp tables are in base/ and user/.

(provide
 make-syntax-table syntax-table?
 syntax-table-parent syntax-table-classes
 set-syntax-table-parent!
 char-syntax
 char-word? char-whitespace? char-open? char-close?
 char-string-quote? char-comment-start? char-escape?
 char-expression-prefix? char-punctuation? char-symbol?)

(struct syntax-table
  ([parent #:mutable] [classes #:mutable]) #:transparent)

(define (make-syntax-table [parent #f])
  (syntax-table parent (make-hash)))

(define (char-syntax ch table)
  (and table
       (or (hash-ref (syntax-table-classes table) ch #f)
           (let ([p (syntax-table-parent table)])
             (and p (char-syntax ch p)))
           'punctuation)))

(define (char-word? ch table)            (eq? (char-syntax ch table) 'word))
(define (char-whitespace? ch table)       (eq? (char-syntax ch table) 'whitespace))
(define (char-open? ch table)            (eq? (char-syntax ch table) 'open))
(define (char-close? ch table)           (eq? (char-syntax ch table) 'close))
(define (char-string-quote? ch table)    (eq? (char-syntax ch table) 'string-quote))
(define (char-comment-start? ch table)   (eq? (char-syntax ch table) 'comment-start))
(define (char-escape? ch table)          (eq? (char-syntax ch table) 'escape))
(define (char-expression-prefix? ch table) (eq? (char-syntax ch table) 'expression-prefix))
(define (char-punctuation? ch table)     (eq? (char-syntax ch table) 'punctuation))
(define (char-symbol? ch table)          (eq? (char-syntax ch table) 'symbol))
