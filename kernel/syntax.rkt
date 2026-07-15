#lang racket

;; kernel/syntax.rkt — Syntax table kernel primitives
;;
;; struct + char-syntax + predicates + per-buffer storage.

(require "buffer.rkt")
;; Standard and Lisp tables are in base/ and user/.

(provide
 make-syntax-table syntax-table?
 syntax-table-parent syntax-table-classes
 syntax-table-multi-rules set-syntax-table-multi-rules!
 set-syntax-table-parent!
 char-syntax
 char-word? char-whitespace? char-open? char-close?
 char-string-quote? char-comment-start? char-escape?
 char-expression-prefix? char-punctuation? char-symbol?
 ;; multi-char comment/string rules
 multi-char-rule multi-char-rule?
 multi-char-rule-tag multi-char-rule-start multi-char-rule-end
 multi-char-rule-nestable? multi-char-rule-delim-capture?
 ;; per-buffer syntax table
 set-buffer-syntax! buffer-syntax-table
 buffer-syntax-version
 ;; cleanup
 syntax-buffer-cleanup!)

;; A multi-char-rule describes a two-character (or longer) delimiter pair.
;;   tag          : symbol used as state name during scanning (e.g. 'block-comment)
;;   start        : string that opens the region (e.g. "#|")
;;   end          : string that closes the region (e.g. "|#"), or #f for delim-capture
;;   nestable?    : #t if nested open/close pairs are counted, #f otherwise
;;   delim-capture?: #t for here-doc style (#<<DELIM ... DELIM), #f for fixed delimiters
(struct multi-char-rule (tag start end nestable? delim-capture?) #:transparent)

(struct syntax-table
  ([parent #:mutable] [classes #:mutable] [multi-rules #:mutable]) #:transparent)

(define (make-syntax-table [parent #f])
  (syntax-table parent (make-hash) '()))

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

;; ============================================================
;; Per-buffer syntax table storage
;; ============================================================
;; A buffer may have a primary syntax-table set via set-buffer-syntax!,
;; and also a fallback via buffer-var 'syntax-table.

(define syntax-table* (make-hasheq))
(define syntax-version-table (make-hasheq))

(define (buffer-syntax-table buf)
  (or (hash-ref syntax-table* buf (λ () #f))
      (buffer-var buf 'syntax-table #f)))

(define (buffer-syntax-version buf)
  (hash-ref syntax-version-table buf (λ () 0)))

(define (set-buffer-syntax! buf st)
  (hash-set! syntax-table* buf st)
  ;; Increment version so that cached parse-states are invalidated.
  (hash-set! syntax-version-table buf
             (add1 (hash-ref syntax-version-table buf (λ () 0)))))

(define (syntax-buffer-cleanup! buf)
  (hash-remove! syntax-table* buf)
  (hash-remove! syntax-version-table buf))
