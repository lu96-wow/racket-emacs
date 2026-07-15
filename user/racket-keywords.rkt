#lang racket

;; modes/racket-keywords.rkt — Racket font-lock keyword table

(require "../kernel/category.rkt")

(provide racket-font-lock-keywords)

(define racket-font-lock-keywords
  `((,(pregexp "\\b(define|lambda|λ|let|let\\*|letrec|let-values|let\\*-values|letrec-values|let-syntax|letrec-syntax|local|shared|recur|case-lambda)\\b") . ,category-keyword)
    (,(pregexp "\\b(if|cond|else|when|unless|and|or|not|case|match)\\b") . ,category-keyword)
    (,(pregexp "\\b(begin|begin0|for|for/list|for/vector|for/hash|for/fold|for/and|for/or|for/first|for/last|for/sum|for/product|in-list|in-range|in-naturals|set!)\\b") . ,category-keyword)
    (,(pregexp "\\b(quote|quasiquote|unquote|unquote-splicing|syntax|quasisyntax)\\b") . ,category-keyword)
    (,(pregexp "\\b(module|module\\+|module\\*|require|provide|all-defined-out|all-from-out|rename-out|except-out|only-in)\\b") . ,category-keyword)
    (,(pregexp "\\b(struct|struct-copy|class|class\\*|interface|interface\\*|mixin|inherit|init|field|public|private|override)\\b") . ,category-keyword)
    (,(pregexp "\\b(define-syntax|define-syntax-rule|define-syntaxes|syntax-rules|syntax-case|with-syntax|syntax-parse)\\b") . ,category-keyword)
    (,(pregexp "\\b(#t|#f|#true|#false|null|empty|void)\\b") . ,category-constant)
    (,(pregexp "\\b[0-9]+(\\.[0-9]+)?([eE][+-]?[0-9]+)?\\b") . ,category-constant)
    (,(pregexp "#:[a-zA-Z_%!@$^&*+=~/<>?-][a-zA-Z0-9_%!@$^&*+=~/<>?-]*") . ,category-builtin)
    (,(pregexp "\\b(cons|car|cdr|list|append|reverse|length|assq|assv|member|memq|memv|remove|remove\\*|sort|list-ref|list-tail|pair\\?|null\\?|list\\?|zero\\?|positive\\?|negative\\?|even\\?|odd\\?|exact\\?|inexact\\?|number\\?|string\\?|symbol\\?|char\\?|boolean\\?|procedure\\?|vector\\?|hash\\?|equal\\?|eq\\?|eqv\\?)\\b") . ,category-builtin)
    (,(pregexp "\\b(apply|map|filter|foldl|foldr|string-append|string-length|string-ref|string->list|string->symbol|string->number|string-join|substring|string\\?|make-string|string-copy|string-replace|regexp-match|regexp-match\\?|regexp-replace|number->string|symbol->string|format|printf|display|displayln|write|print|read|read-line|open-input-file|open-output-file|call-with-input-file|call-with-output-file|port\\?)\\b") . ,category-builtin)
    (,(pregexp "\\b(with-handlers|raise|error|make-parameter|parameterize|parameterize\\*|call/cc|call-with-current-continuation|dynamic-wind|exit|make-hash|make-hasheq|make-hasheqv|hash-ref|hash-set|hash-remove|hash-has-key|hash-count|hash-keys|hash-values|hash-iterate|in-hash|for/hash|call-with-values)\\b") . ,category-builtin)
    (,(pregexp "(?<=\\(define\\s)\\S+") . ,category-function-name)
    (,(pregexp "(?<=\\((lambda|λ)\\s)\\S+") . ,category-variable-name)))
