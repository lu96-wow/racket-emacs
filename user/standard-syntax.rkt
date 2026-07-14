#lang racket

;; base/syntax.rkt — Standard syntax table (letters, digits, _ = word)

(require "../kernel/syntax.rkt")

(provide make-standard-syntax-table)

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
