#lang racket

;; lang/racket-lang.rkt — Racket language definition (pure data)
;;
;; Exports a complete lang-def for Racket: face colours, keyword
;; patterns, and syntax-table.  No imperative code — just data.

(require "syntax.rkt"
         "define.rkt"
         "../display/face.rkt")

(provide racket-lang-def)

;; ============================================================
;; Face definitions
;; ============================================================

(define racket-faces
  (list
   (list 'font-lock-comment-face
         (make-face-attrs 'foreground (list 100 160 100) 'slant 'italic))
   (list 'font-lock-string-face
         (make-face-attrs 'foreground (list 80 180 80)))
   (list 'font-lock-keyword-face
         (make-face-attrs 'foreground (list 50 150 255) 'weight 'bold))
   (list 'font-lock-builtin-face
         (make-face-attrs 'foreground (list 50 150 255)))
   (list 'font-lock-constant-face
         (make-face-attrs 'foreground (list 220 80 80)))
   (list 'font-lock-type-face
         (make-face-attrs 'foreground (list 200 180 60) 'weight 'bold))
   (list 'font-lock-function-name-face
         (make-face-attrs 'weight 'bold))))

;; ============================================================
;; Keyword patterns
;; ============================================================

;; Helper: build keyword entry from a face name and regex alternation
(define (regexp-list->keyword-list face . rx-parts)
  (list (cons (pregexp (apply string-append rx-parts)) face)))

(define racket-keywords
  (append
   ;; ── Special forms ──
   (regexp-list->keyword-list
    'font-lock-keyword-face
    "\\b(define|lambda|λ|let|let\\*|letrec|let-values|let\\*-values"
    "|if|cond|case|and|or|when|unless|begin"
    "|set!|quote|quasiquote|unquote|unquote-splicing"
    "|syntax|quasisyntax|unsyntax|unsyntax-splicing"
    "|syntax-rules|syntax-case|with-syntax"
    "|parameterize|dynamic-wind"
    "|call/cc|call-with-current-continuation|call-with-values"
    "|let/cc|let/ec)\\b")

   ;; ── Definition forms ──
   (regexp-list->keyword-list
    'font-lock-keyword-face
    "\\b(define-syntax|define-syntax-rule|define-syntax-parse-rule"
    "|define-simple-macro|define-for-syntax"
    "|struct|define-struct|define-values"
    "|define-match-expander|syntax-parse|define/contract)\\b")

   ;; ── Module forms ──
   (regexp-list->keyword-list
    'font-lock-keyword-face
    "\\b(module|module\\+|module\\*|require|provide"
    "|all-defined-out|all-from-out|except-out|rename-out"
    "|prefix-out|contract-out|only-in|except-in"
    "|rename-in|prefix-in|relative-in|submod"
    "|for-meta|for-syntax|for-template|for-label|for-space)\\b")

   ;; ── Loops / Comprehensions ──
   (regexp-list->keyword-list
    'font-lock-keyword-face
    "\\b(for|for/list|for/vector|for/hash|for/and|for/or"
    "|for/sum|for/product|for/fold|for/first|for/last|for/foldr"
    "|for\\*|for/list\\*|for/vector\\*|for/fold\\*"
    "|in-list|in-range|in-naturals|in-hash|in-vector|in-string"
    "|in-directory|in-sequences|in-parallel|in-indexed"
    "|stop-before|stop-after|sequence-generate)\\b")

   ;; ── Match ──
   (regexp-list->keyword-list
    'font-lock-keyword-face
    "\\b(match|match-define|match-let|match-let\\*"
    "|match-lambda|match-lambda\\*|match/values)\\b")

   ;; ── Class / Object ──
   (regexp-list->keyword-list
    'font-lock-keyword-face
    "\\b(class|class\\*|class/derived|interface|interface\\*"
    "|mixin|trait|new|send|send\\*|send/apply|field"
    "|super-new|super-make-object|init-field"
    "|define/public|define/private|define/augment|define/override"
    "|augment|inherit-field|this%)\\b")

   ;; ── Contract ──
   (regexp-list->keyword-list
    'font-lock-keyword-face
    "\\b(->|->\\*|->i|->d|case->|->m"
    "|or/c|and/c|not/c|listof|vectorof|cons/c|hash/c"
    "|between/c|real-in|integer-in|natural-number/c"
    "|exact-positive-integer\\?|exact-nonnegative-integer\\?"
    "|string\\?|number\\?|boolean\\?|symbol\\?|procedure\\?"
    "|any/c|none/c)\\b")

   ;; ── Testing ──
   (regexp-list->keyword-list
    'font-lock-keyword-face
    "\\b(check-equal\\?|check-true|check-false|check-not-equal\\?"
    "|check-exn|check-match|test-case|test-begin|test-end"
    "|define-check|rackunit|test-suite)\\b")

   ;; ── Type annotations ──
   (regexp-list->keyword-list
    'font-lock-type-face
    "\\b(Any|Number|String|Symbol|Boolean|Listof|Vectorof"
    "|HashTable|Procedure|Void|U|Integer|Natural|Real|Complex)\\b")

   ;; ── Constants ──
   (regexp-list->keyword-list
    'font-lock-constant-face
    "\\b(#t|#f|#true|#false|null|empty)\\b")

   ;; ── Builtin functions ──
   (regexp-list->keyword-list
    'font-lock-builtin-face
    "\\b(cons|car|cdr|cadr|cddr|caar|cdar|list|list\\*|append|reverse"
    "|length|map|filter|foldl|foldr|andmap|ormap"
    "|memq|memv|member|assq|assv|assoc"
    "|remove|remq|remv|remq\\*|remv\\*|sort|quicksort"
    "|first|second|third|fourth|fifth|sixth|seventh|eighth"
    "|rest|last|take|drop|split-at|flatten|range"
    "|add1|sub1|zero\\?|positive\\?|negative\\?"
    "|even\\?|odd\\?|exact\\?|inexact\\?"
    "|integer\\?|rational\\?|real\\?|complex\\?"
    "|char\\?|string\\?|bytes\\?|symbol\\?|keyword\\?"
    "|list\\?|pair\\?|null\\?|void\\?|eof-object\\?"
    "|struct\\?|vector\\?|hash\\?|box\\?|procedure\\?"
    "|input-port\\?|output-port\\?|port\\?|path\\?"
    "|identifier\\?|syntax\\?)\\b")

   ;; ── I/O ──
   (regexp-list->keyword-list
    'font-lock-builtin-face
    "\\b(display|displayln|print|printf|fprintf|write|read|read-line"
    "|open-input-file|open-output-file"
    "|call-with-input-file|call-with-output-file"
    "|with-input-from-file|with-output-to-file"
    "|close-input-port|close-output-port"
    "|port->string|file->string|string->file"
    "|current-input-port|current-output-port|current-error-port"
    "|eof|eof-object\\?)\\b")))

;; ============================================================
;; lang-def — the complete Racket language definition
;; ============================================================

(define racket-lang-def
  (lang-def 'racket
            '(".rkt" ".scrbl" ".rktl" ".rktd")
            (make-racket-syntax-table)
            racket-keywords
            racket-faces))


