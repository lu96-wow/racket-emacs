#lang racket

;; kernel/data/syntax.rkt — Syntax Table (pure data, zero dependencies)
;;
;; ============================================================================
;; Maps characters to syntax classes and defines multi-character delimiter
;; rules (block comments, here-strings, etc.).  Parent inheritance for
;; fallback — walks the parent chain until a class is found.
;;
;; ============================================================================
;; Syntax Classes (9 total)
;; ============================================================================
;;
;;   'word              — identifier constituent (a–z, A–Z, 0–9, -, _, etc.)
;;   'whitespace        — space, tab, newline, carriage return
;;   'open              — opening delimiter: ( [ {
;;   'close             — closing delimiter: ) ] }
;;   'string-quote      — string delimiter: "
;;   'comment-start     — line comment start: ;
;;   'escape            — escape character: \
;;   'expression-prefix — quote-like: ' ` ,
;;   'punctuation       — everything else (default fallback)
;;
;; ============================================================================
;; Multi-Character Rules
;; ============================================================================
;;
;;   tag            — symbol naming the rule type: 'block-comment, 'here-string
;;   start-str      — string that opens the region, e.g. "#|"
;;   end-str        — string that closes, or #f for heredoc (delim-capture)
;;   nestable?      — can this region contain nested occurrences? (#|...|#)
;;   delim-capture? — is the closer determined from the opener? (#<<HERE)
;;
;; ============================================================================
;; Computation Only — all functions are pure queries
;; ============================================================================

(provide
 ;; ── struct ──
 syntax-table? make-syntax-table
 syntax-table-parent syntax-table-classes syntax-table-multi-rules
 set-syntax-table-parent! set-syntax-table-classes! set-syntax-table-multi-rules!

 ;; ── multi-char rule ──
 multi-char-rule? multi-char-rule
 multi-char-rule-tag
 multi-char-rule-start-str multi-char-rule-end-str
 multi-char-rule-nestable? multi-char-rule-delim-capture?

 ;; ── char classification (pure, walks parent chain) ──
 char-syntax
 char-word? char-whitespace? char-open? char-close?
 char-string-quote? char-string-delimiter? char-comment-start? char-escape?
 char-expression-prefix? char-symbol-constituent? char-punctuation?

 ;; ── predefined tables ──
 make-standard-syntax-table    ; Lisp/Racket base
 make-racket-syntax-table)     ; Standard + Racket multi-char rules

;; ============================================================
;; Syntax table
;; ============================================================

(struct syntax-table
  ([parent #:mutable]         ; (or/c syntax-table? #f) — fallback chain
   [classes #:mutable]        ; (hash/c char? symbol?) — char → class
   [multi-rules #:mutable])   ; (listof multi-char-rule?)
  #:transparent)

(define (make-syntax-table [parent #f])
  (unless (or (not parent) (syntax-table? parent))
    (raise-argument-error 'make-syntax-table
                          "(or/c syntax-table? #f)" parent))
  (syntax-table parent (make-hash) '()))

;; ============================================================
;; Multi-character rule
;; ============================================================

(struct multi-char-rule
  (tag              ; symbol — 'block-comment | 'here-string
   start-str        ; string — opens the region
   end-str          ; (or/c string? #f) — closes the region
   nestable?        ; boolean? — count nested open/close pairs?
   delim-capture?)  ; boolean? — heredoc-style delimiter capture?
  #:transparent)

;; ============================================================
;; Char Classification — walks parent chain, ultimate default = 'punctuation
;; ============================================================

(define (char-syntax ch table)
  (and table
       (or (hash-ref (syntax-table-classes table) ch #f)
           (let ([p (syntax-table-parent table)])
             (and p (char-syntax ch p)))
           'punctuation)))

(define (char-word? ch table)            (eq? (char-syntax ch table) 'word))
(define (char-whitespace? ch table)      (eq? (char-syntax ch table) 'whitespace))
(define (char-open? ch table)            (eq? (char-syntax ch table) 'open))
(define (char-close? ch table)           (eq? (char-syntax ch table) 'close))
(define (char-string-quote? ch table)    (eq? (char-syntax ch table) 'string-quote))
(define (char-comment-start? ch table)   (eq? (char-syntax ch table) 'comment-start))
(define (char-escape? ch table)          (eq? (char-syntax ch table) 'escape))
(define (char-expression-prefix? ch table) (eq? (char-syntax ch table) 'expression-prefix))
(define (char-punctuation? ch table)     (eq? (char-syntax ch table) 'punctuation))

(define (char-string-delimiter? ch table)
  ;; Alternative string delimiters (for heredoc-like syntax).
  ;; Default: same as string-quote.  Override in derived tables.
  (eq? (char-syntax ch table) 'string-delimiter))

(define (char-symbol-constituent? ch table)
  ;; Characters that are part of symbol names but not words.
  ;; Default: same as word.  Override in derived tables.
  (eq? (char-syntax ch table) 'symbol-constituent))

;; ============================================================
;; Standard Lisp syntax table
;; ============================================================

(define (make-standard-syntax-table)
  (define st (make-syntax-table))
  (define h (syntax-table-classes st))

  ;; Word constituents — alphanumeric + common Lisp symbol chars
  (for ([ch (in-string "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!#$%&*+-./:<=>?@^_~")])
    (hash-set! h ch 'word))

  ;; Whitespace
  (for ([ch (list #\space #\tab #\newline #\return #\page)])
    (hash-set! h ch 'whitespace))

  ;; Delimiters
  (hash-set! h #\( 'open)  (hash-set! h #\) 'close)
  (hash-set! h #\[ 'open)  (hash-set! h #\] 'close)
  (hash-set! h #\{ 'open)  (hash-set! h #\} 'close)

  ;; String and escape
  (hash-set! h #\" 'string-quote)
  (hash-set! h #\\ 'escape)

  ;; Comment
  (hash-set! h #\; 'comment-start)

  ;; Expression prefix (quote / quasiquote / unquote)
  (hash-set! h #\' 'expression-prefix)
  (hash-set! h #\` 'expression-prefix)
  (hash-set! h #\, 'expression-prefix)

  st)

;; ============================================================
;; Racket syntax table — inherits standard, adds block-comment + heredoc
;; ============================================================

(define (make-racket-syntax-table)
  (define st (make-syntax-table (make-standard-syntax-table)))
  (set-syntax-table-multi-rules! st
    (list
     ;; #| ... |#  block comment (nestable — inner #| increments depth)
     (multi-char-rule 'block-comment "#|" "|#" #t #f)
     ;; #<<HERE ... HERE  heredoc string (delim-capture from opener)
     (multi-char-rule 'here-string "#<<" #f #f #t)))
  st)
