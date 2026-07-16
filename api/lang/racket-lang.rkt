#lang racket

;; api/lang/racket-lang.rkt — Racket language font-lock configuration
;;
;; Provides Racket-specific syntax table, keyword patterns, and
;; face definitions.  Called by main.rkt to set up Racket buffers.

(require "../../kernel/syntax.rkt"
         "../../kernel/buffer.rkt"
         "../../base/font-lock.rkt"
         "../../display/face.rkt")

(provide
 racket-syntax-table
 racket-font-lock-keywords
 setup-racket-lang!
 define-racket-font-lock-faces!)

;; ============================================================
;; Face definitions
;; ============================================================

(define (define-racket-font-lock-faces!)
  ;; Only define if not already registered
  (define-face! 'font-lock-comment-face
    (make-face-attrs 'foreground (list 100 160 100) 'slant 'italic))
  (define-face! 'font-lock-string-face
    (make-face-attrs 'foreground (list 80 180 80)))
  (define-face! 'font-lock-keyword-face
    (make-face-attrs 'foreground (list 50 150 255) 'weight 'bold))
  (define-face! 'font-lock-builtin-face
    (make-face-attrs 'foreground (list 50 150 255)))
  (define-face! 'font-lock-constant-face
    (make-face-attrs 'foreground (list 220 80 80)))
  (define-face! 'font-lock-type-face
    (make-face-attrs 'foreground (list 200 180 60) 'weight 'bold))
  (define-face! 'font-lock-function-name-face
    (make-face-attrs 'weight 'bold)))

;; ============================================================
;; Syntax table
;; ============================================================

(define racket-syntax-table (make-racket-syntax-table))

;; ============================================================
;; Keyword patterns — order = priority (first match wins)
;; ============================================================

(define racket-font-lock-keywords
  (list
   ;; Special forms
   (cons (pregexp "\\b(define|lambda|λ|let|let\\*|letrec|let-values|let\\*-values|if|cond|case|and|or|when|unless|begin|set!|quote|quasiquote|unquote|unquote-splicing|syntax|quasisyntax|unsyntax|unsyntax-splicing|syntax-rules|syntax-case|with-syntax|parameterize|dynamic-wind|call/cc|call-with-current-continuation|call-with-values|let/cc|let/ec)\\b")
         'font-lock-keyword-face)

   ;; Definition forms
   (cons (pregexp "\\b(define-syntax|define-syntax-rule|define-syntax-parse-rule|define-simple-macro|define-for-syntax|struct|define-struct|define-values|define-match-expander|syntax-parse|define/contract)\\b")
         'font-lock-keyword-face)

   ;; Module forms
   (cons (pregexp "\\b(module|module\\+|module\\*|require|provide|all-defined-out|all-from-out|except-out|rename-out|prefix-out|contract-out|for-meta|for-syntax|for-template|for-label|for-space|only-in|except-in|rename-in|prefix-in|relative-in|submod)\\b")
         'font-lock-keyword-face)

   ;; Control flow
   (cons (pregexp "\\b(for|for/list|for/vector|for/hash|for/and|for/or|for/sum|for/product|for/fold|for/first|for/last|for/foldr|for\\*|for/list\\*|for/vector\\*|for/fold\\*|in-list|in-range|in-naturals|in-hash|in-vector|in-string|in-directory|in-sequences|in-parallel|in-indexed|stop-before|stop-after|sequence-generate)\\b")
         'font-lock-keyword-face)

   ;; Match
   (cons (pregexp "\\b(match|match-define|match-let|match-let\\*|match-lambda|match-lambda\\*|match/values)\\b")
         'font-lock-keyword-face)

   ;; Class
   (cons (pregexp "\\b(class|class\\*|class/derived|interface|interface\\*|mixin|trait|new|send|send\\*|send/apply|field|super-new|super-make-object|init-field|define/public|define/private|define/augment|define/override|augment|inherit-field|this%)\\b")
         'font-lock-keyword-face)

   ;; Contract
   (cons (pregexp "\\b(->|->\\*|->i|->d|case->|->m|or/c|and/c|not/c|listof|vectorof|cons/c|hash/c|between/c|real-in|integer-in|natural-number/c|exact-positive-integer\\?|exact-nonnegative-integer\\?|string\\?|number\\?|boolean\\?|symbol\\?|procedure\\?|any/c|none/c)\\b")
         'font-lock-keyword-face)

   ;; Testing
   (cons (pregexp "\\b(check-equal\\?|check-true|check-false|check-not-equal\\?|check-exn|check-match|test-case|test-begin|test-end|define-check|rackunit|test-suite)\\b")
         'font-lock-keyword-face)

   ;; Type annotations
   (cons (pregexp "\\b(: |:: |Exercise|Any|Number|String|Symbol|Boolean|Listof|Vectorof|HashTable|Procedure|Void|U|case->|->|Integer|Natural|Real|Complex)\\b")
         'font-lock-type-face)

   ;; Constants
   (cons (pregexp "\\b(#t|#f|#true|#false|null|empty|#\\\\(|#\\\\))\\b")
         'font-lock-constant-face)

   ;; Builtins
   (cons (pregexp "\\b(cons|car|cdr|cadr|cddr|caar|cdar|list|list\\*|append|reverse|length|map|filter|foldl|foldr|andmap|ormap|memq|memv|member|assq|assv|assoc|remove|remq|remv|remq\\*|remv\\*|sort|quicksort|first|second|third|fourth|fifth|sixth|seventh|eighth|rest|last|take|drop|split-at|flatten|range|add1|sub1|zero\\?|positive\\?|negative\\?|even\\?|odd\\?|exact\\?|inexact\\?|integer\\?|rational\\?|real\\?|complex\\?|char\\?|string\\?|bytes\\?|symbol\\?|keyword\\?|list\\?|pair\\?|null\\?|void\\?|eof-object\\?|struct\\?|vector\\?|hash\\?|box\\?|procedure\\?|input-port\\?|output-port\\?|port\\?|path\\?|identifier\\?|syntax\\?)\\b")
         'font-lock-builtin-face)

   ;; Lambda
   (cons (pregexp "\\b(lambda|λ|case-lambda)\\b")
         'font-lock-keyword-face)

   ;; I/O
   (cons (pregexp "\\b(display|displayln|print|printf|fprintf|write|read|read-line|open-input-file|open-output-file|call-with-input-file|call-with-output-file|with-input-from-file|with-output-to-file|close-input-port|close-output-port|port->string|file->string|string->file|current-input-port|current-output-port|current-error-port|eof|eof-object\\?)\\b")
         'font-lock-builtin-face)))

;; ============================================================
;; Setup function
;; ============================================================

(define (setup-racket-lang! buf)
  (set-buffer-syntax-table! buf racket-syntax-table)
  (set-buffer-font-lock-config! buf
    (make-font-lock-config
     #:keywords racket-font-lock-keywords
     #:syntax? #t
     #:case-fold? #f))
  ;; Full initial fontification
  (fontify-region! buf 0 (buffer-length buf)))
