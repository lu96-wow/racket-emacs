#lang racket

;; lang/python-lang.rkt — Python language definition (pure data)
;;
;; Demonstrates that the framework supports non-Lisp languages.
;; Python has different quoting rules (triple-quoted strings),
;; single-line # comments, and very different keyword set.

(require "syntax.rkt"
         "define.rkt"
         "../display/face.rkt")

(provide python-lang-def)

;; ============================================================
;; Python syntax table
;; ============================================================

(define (make-python-syntax-table)
  (define st (make-syntax-table))
  (define h (syntax-table-classes st))

  ;; Word constituents
  (for ([ch (in-string "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_")])
    (hash-set! h ch 'word))

  ;; Whitespace
  (for ([ch (list #\space #\tab #\newline #\return)])
    (hash-set! h ch 'whitespace))

  ;; Delimiters
  (hash-set! h #\( 'open)  (hash-set! h #\) 'close)
  (hash-set! h #\[ 'open)  (hash-set! h #\] 'close)
  (hash-set! h #\{ 'open)  (hash-set! h #\} 'close)

  ;; String quotes (single, double, and triple for multi-line)
  (hash-set! h #\" 'string-quote)
  (hash-set! h #\' 'string-quote)

  ;; Escape
  (hash-set! h #\\ 'escape)

  ;; Comment: # to end of line
  (hash-set! h #\# 'comment-start)

  ;; Multi-char rules: triple-quoted strings (""") and (''')
  ;; These are block-like delimiters — nestable, end on matching triple
  (set-syntax-table-multi-rules! st
    (list
     ;; Note: simplified — real Python triple-quote handling is more complex
     ;; (doesn't nest, can be single or double quote).  This is a demo.
     (multi-char-rule 'block-string "\"\"\"" "\"\"\"" #f #f)
     (multi-char-rule 'block-string "'''" "'''" #f #f)))

  st)

;; ============================================================
;; Face definitions
;; ============================================================

(define python-faces
  (list
   (list 'font-lock-comment-face
         (make-face-attrs 'foreground (list 100 160 100) 'slant 'italic))
   (list 'font-lock-string-face
         (make-face-attrs 'foreground (list 210 160 80)))
   (list 'font-lock-keyword-face
         (make-face-attrs 'foreground (list 255 140 60) 'weight 'bold))
   (list 'font-lock-builtin-face
         (make-face-attrs 'foreground (list 80 180 220)))
   (list 'font-lock-constant-face
         (make-face-attrs 'foreground (list 180 120 255)))
   (list 'font-lock-type-face
         (make-face-attrs 'foreground (list 80 220 180) 'weight 'bold))
   (list 'font-lock-function-name-face
         (make-face-attrs 'foreground (list 220 200 100)))))

;; ============================================================
;; Keyword patterns
;; ============================================================

(define python-keywords
  (list
   ;; Control flow
   (cons (pregexp
          "\\b(if|elif|else|for|while|break|continue|pass|return|yield|raise|try|except|finally|with|as|assert|match|case)\\b")
         'font-lock-keyword-face)

   ;; Definitions
   (cons (pregexp "\\b(def|class|lambda|async|await)\\b")
         'font-lock-keyword-face)

   ;; Module
   (cons (pregexp "\\b(import|from|as|global|nonlocal|del)\\b")
         'font-lock-keyword-face)

   ;; Boolean
   (cons (pregexp "\\b(True|False|None)\\b")
         'font-lock-constant-face)

   ;; Builtins (subset)
   (cons (pregexp
          (string-append
           "\\b(print|len|range|enumerate|zip|map|filter|sorted|reversed"
           "|any|all|sum|min|max|abs|round|int|str|float|bool|list|dict|set|tuple"
           "|type|isinstance|issubclass|hasattr|getattr|setattr|delattr"
           "|open|input|super|property|staticmethod|classmethod"
           "|Exception|ValueError|TypeError|KeyError|IndexError"
           "|RuntimeError|StopIteration|OSError|IOError|FileNotFoundError)\\b"))
         'font-lock-builtin-face)

   ;; Type hints
   (cons (pregexp "\\b(List|Dict|Set|Tuple|Optional|Union|Callable|Iterable|Iterator|Any)\\b")
         'font-lock-type-face)

   ;; def name
   (cons (pregexp "\\bdef[ \t]+\\([a-zA-Z_][a-zA-Z0-9_]+\\)")
         'font-lock-function-name-face)))

;; ============================================================
;; lang-def
;; ============================================================

(define python-lang-def
  (lang-def 'python
            '(".py" ".pyw" ".pyi")
            (make-python-syntax-table)
            python-keywords
            python-faces))


