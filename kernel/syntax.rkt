#lang racket

;; core/syntax.rkt — Syntax table: char → syntax-class with parent chain
;;
;; Used by word-movement, paren-matching, etc.
;; Dependency-free.

(provide
 ;; syntax-table
 make-syntax-table syntax-table?
 syntax-table-parent syntax-table-classes
 set-syntax-table-parent!

 ;; class query
 char-syntax
 char-word? char-whitespace? char-open? char-close?
 char-string-quote? char-comment-start? char-escape?
 char-expression-prefix? char-punctuation? char-symbol?

 ;; standard tables
 make-standard-syntax-table
 make-lisp-syntax-table)

(define valid-classes
  '(word whitespace open close
    string-quote comment-start escape
    expression-prefix punctuation symbol))

(struct syntax-table
  ([parent #:mutable]   ; syntax-table | #f
   [classes #:mutable]) ; (hash/c char? symbol?)
  #:transparent)

(define (make-syntax-table [parent #f])
  (syntax-table parent (make-hash)))

(define (char-syntax ch table)
  (or (hash-ref (syntax-table-classes table) ch #f)
      (let ([p (syntax-table-parent table)])
        (and p (char-syntax ch p)))
      'punctuation))

;; Predicates
(define (char-word? ch table)      (eq? (char-syntax ch table) 'word))
(define (char-whitespace? ch table) (eq? (char-syntax ch table) 'whitespace))
(define (char-open? ch table)      (eq? (char-syntax ch table) 'open))
(define (char-close? ch table)     (eq? (char-syntax ch table) 'close))
(define (char-string-quote? ch table) (eq? (char-syntax ch table) 'string-quote))
(define (char-comment-start? ch table) (eq? (char-syntax ch table) 'comment-start))
(define (char-escape? ch table)    (eq? (char-syntax ch table) 'escape))
(define (char-expression-prefix? ch table) (eq? (char-syntax ch table) 'expression-prefix))
(define (char-punctuation? ch table) (eq? (char-syntax ch table) 'punctuation))
(define (char-symbol? ch table)    (eq? (char-syntax ch table) 'symbol))

;; Standard syntax table
(define (make-standard-syntax-table)
  (define table (make-syntax-table))
  (define classes (syntax-table-classes table))
  (for ([c (in-range 48 58)])  (hash-set! classes (integer->char c) 'word))
  (for ([c (in-range 65 91)])  (hash-set! classes (integer->char c) 'word))
  (for ([c (in-range 97 123)]) (hash-set! classes (integer->char c) 'word))
  (hash-set! classes #\_ 'word)
  (hash-set! classes #\space 'whitespace)
  (hash-set! classes #\tab 'whitespace)
  (hash-set! classes #\newline 'whitespace)
  (hash-set! classes #\return 'whitespace)
  (hash-set! classes #\( 'open)  (hash-set! classes #\) 'close)
  (hash-set! classes #\[ 'open)  (hash-set! classes #\] 'close)
  (hash-set! classes #\{ 'open)  (hash-set! classes #\} 'close)
  (hash-set! classes #\" 'string-quote)
  (hash-set! classes #\\ 'escape)
  table)

;; Lisp syntax table (inherits standard)
(define (make-lisp-syntax-table)
  (define table (make-syntax-table (make-standard-syntax-table)))
  (define classes (syntax-table-classes table))
  (hash-set! classes #\' 'expression-prefix)
  (hash-set! classes #\` 'expression-prefix)
  (hash-set! classes #\, 'expression-prefix)
  (hash-set! classes #\# 'expression-prefix)
  (hash-set! classes #\; 'comment-start)
  (hash-set! classes #\| 'symbol)
  table)
