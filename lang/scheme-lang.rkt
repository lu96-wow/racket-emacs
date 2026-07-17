#lang racket

;; lang/scheme-lang.rkt — Scheme language definition (pure data)

(require "syntax.rkt"
         "define.rkt"
         "../display/face.rkt")

(provide scheme-lang-def)

(define scheme-faces
  (list
   (list 'font-lock-comment-face
         (make-face-attrs 'foreground (list 180 80 80) 'slant 'italic))
   (list 'font-lock-string-face
         (make-face-attrs 'foreground (list 180 160 80)))
   (list 'font-lock-keyword-face
         (make-face-attrs 'foreground (list 80 140 255) 'weight 'bold))
   (list 'font-lock-builtin-face
         (make-face-attrs 'foreground (list 80 140 255)))
   (list 'font-lock-constant-face
         (make-face-attrs 'foreground (list 255 100 100)))))

(define scheme-keywords
  (list
   (cons (pregexp
          (string-append
           "\\b(define|lambda|λ|let|let\\*|letrec|let-values|let\\*-values"
           "|if|cond|case|and|or|when|unless|begin|do"
           "|set!|quote|quasiquote|unquote|unquote-splicing"
           "|syntax-rules|syntax-case|with-syntax"
           "|parameterize|dynamic-wind"
           "|call/cc|call-with-current-continuation"
           "|call-with-values|delay|force"
           "|make-parameter|define-syntax|let-syntax|letrec-syntax"
           "|define-record-type)\\b"))
         'font-lock-keyword-face)
   (cons (pregexp
          (string-append
           "\\b(cons|car|cdr|cadr|cddr|caar|cdar|list|list\\*|append|reverse"
           "|length|map|for-each|filter|foldl|foldr|andmap|ormap"
           "|memq|memv|member|assq|assv|assoc"
           "|remove|remq|remv|sort"
           "|first|second|third|fourth|rest|last|take|drop"
           "|add1|sub1|zero\\?|positive\\?|negative\\?"
           "|even\\?|odd\\?|exact\\?|inexact\\?"
           "|integer\\?|rational\\?|real\\?|complex\\?"
           "|number\\?|char\\?|string\\?|bytes\\?|symbol\\?"
           "|list\\?|pair\\?|null\\?|void\\?|eof-object\\?"
           "|vector\\?|procedure\\?|port\\?|input-port\\?|output-port\\?"
           "|eq\\?|eqv\\?|equal\\?)\\b"))
         'font-lock-builtin-face)
   (cons (pregexp "\\b(#t|#f|#true|#false|'()|'#())\\b")
         'font-lock-constant-face)
   (cons (pregexp
          (string-append
           "\\b(display|displayln|print|write|read|read-line|read-char"
           "|newline|open-input-file|open-output-file"
           "|call-with-input-file|call-with-output-file"
           "|with-input-from-file|with-output-to-file"
           "|close-input-port|close-output-port"
           "|current-input-port|current-output-port|current-error-port"
           "|eof-object|eof-object\\?|char-ready\\?|peek-char)\\b"))
         'font-lock-builtin-face)))

(define scheme-lang-def
  (lang-def 'scheme
            '(".scm" ".ss" ".sch")
            (make-standard-syntax-table)  ; no block comments or heredoc
            scheme-keywords
            scheme-faces))


