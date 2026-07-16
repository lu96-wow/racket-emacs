#lang racket

;; kernel/syntax.rkt — Syntax Table (pure data, zero dependencies)
;;
;; Maps characters to syntax classes and defines multi-character
;; delimiter rules (block comments, here-strings, etc.).
;; Parent inheritance for fallback.
;;
;; 9 syntax classes:
;;   word  whitespace  open  close  string-quote
;;   comment-start  escape  expression-prefix  punctuation

(provide
 ;; struct
 syntax-table? make-syntax-table
 syntax-table-parent syntax-table-classes syntax-table-multi-rules
 set-syntax-table-parent! set-syntax-table-classes! set-syntax-table-multi-rules!

 ;; multi-char rule
 multi-char-rule? multi-char-rule
 multi-char-rule-tag
 multi-char-rule-start-str multi-char-rule-end-str
 multi-char-rule-nestable? multi-char-rule-delim-capture?

 ;; char classification
 char-syntax
 char-word? char-whitespace? char-open? char-close?
 char-string-quote? char-comment-start? char-escape?
 char-expression-prefix? char-punctuation?

 ;; predefined tables
 make-standard-syntax-table    ; Lisp/Racket base
 make-racket-syntax-table)     ; Standard + Racket multi-char rules

;; ============================================================
;; Syntax table
;; ============================================================

(struct syntax-table
  ([parent #:mutable]         ; (or/c syntax-table? #f)
   [classes #:mutable]        ; (hash/c char? symbol?)
   [multi-rules #:mutable])   ; (listof multi-char-rule?)
  #:transparent)

(define (make-syntax-table [parent #f])
  (syntax-table parent (make-hash) '()))

;; ============================================================
;; Multi-character rule
;; ============================================================

(struct multi-char-rule
  (tag              ; symbol — state name: 'block-comment, 'here-string
   start-str        ; string — opens the region, e.g. "#|"
   end-str          ; string | #f — closes the region, e.g. "|#" (or #f for heredoc)
   nestable?        ; boolean? — count nested open/close?
   delim-capture?)  ; boolean? — heredoc style (#<<DELIM)?
  #:transparent)

;; ============================================================
;; Char classification — walks parent chain
;; ============================================================

(define (char-syntax ch table)
  (and table
       (or (hash-ref (syntax-table-classes table) ch #f)
           (let ([p (syntax-table-parent table)])
             (and p (char-syntax ch p)))
           'punctuation)))  ; ultimate default

(define (char-word? ch table)            (eq? (char-syntax ch table) 'word))
(define (char-whitespace? ch table)      (eq? (char-syntax ch table) 'whitespace))
(define (char-open? ch table)            (eq? (char-syntax ch table) 'open))
(define (char-close? ch table)           (eq? (char-syntax ch table) 'close))
(define (char-string-quote? ch table)    (eq? (char-syntax ch table) 'string-quote))
(define (char-comment-start? ch table)   (eq? (char-syntax ch table) 'comment-start))
(define (char-escape? ch table)          (eq? (char-syntax ch table) 'escape))
(define (char-expression-prefix? ch table) (eq? (char-syntax ch table) 'expression-prefix))
(define (char-punctuation? ch table)     (eq? (char-syntax ch table) 'punctuation))

;; ============================================================
;; Standard Lisp syntax table
;; ============================================================

(define (make-standard-syntax-table)
  (define st (make-syntax-table))
  (define h (syntax-table-classes st))

  ;; Word constituents
  (for ([ch (in-string "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!#$%&*+-./:<=>?@^_~")])
    (hash-set! h ch 'word))

  ;; Whitespace
  (for ([ch (list #\space #\tab #\newline #\return #\page)])
    (hash-set! h ch 'whitespace))

  ;; Delimiters
  (hash-set! h #\( 'open)
  (hash-set! h #\) 'close)
  (hash-set! h #\[ 'open)
  (hash-set! h #\] 'close)
  (hash-set! h #\{ 'open)
  (hash-set! h #\} 'close)

  ;; String and escape
  (hash-set! h #\" 'string-quote)
  (hash-set! h #\\ 'escape)

  ;; Comment
  (hash-set! h #\; 'comment-start)

  ;; Expression prefix
  (hash-set! h #\' 'expression-prefix)
  (hash-set! h #\` 'expression-prefix)
  (hash-set! h #\, 'expression-prefix)

  st)

;; ============================================================
;; Racket syntax table — standard + block-comment + here-string
;; ============================================================

(define (make-racket-syntax-table)
  (define st (make-syntax-table (make-standard-syntax-table)))
  (set-syntax-table-multi-rules! st
    (list
     ;; #| ... |#  block comment (nestable)
     (multi-char-rule 'block-comment "#|" "|#" #t #f)
     ;; #<<HERE ... HERE  heredoc string (delim-capture)
     (multi-char-rule 'here-string "#<<" #f #f #t)))
  st)

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)

  (let ([st (make-standard-syntax-table)])
    (check-true (char-word? #\a st))
    (check-true (char-word? #\? st))
    (check-true (char-whitespace? #\space st))
    (check-true (char-open? #\( st))
    (check-true (char-close? #\) st))
    (check-true (char-string-quote? #\" st))
    (check-true (char-escape? #\\ st))
    (check-true (char-comment-start? #\; st))
    (check-true (char-expression-prefix? #\' st))
    (check-false (char-word? #\新 st))  ; CJK defaults to punctuation
    (check-true (char-punctuation? #\新 st)))

  (let ([st (make-racket-syntax-table)])
    (define rules (syntax-table-multi-rules st))
    (check-equal? (length rules) 2)
    (check-equal? (multi-char-rule-tag (car rules)) 'block-comment)
    (check-true (multi-char-rule-nestable? (car rules)))
    ;; parent inheritance
    (check-true (char-word? #\a st))
    (check-true (char-open? #\( st)))
)
