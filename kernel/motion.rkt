#lang racket

;; kernel/motion.rkt — Pure scanning primitives (gap-buffer + syntax-table)
;;
;; All functions are pure: gap-buffer × byte-position × syntax-table → new position.
;; No dependency on dirty-buffer, buffer, kill-ring, or any mutable state.
;; Independently testable without wiring up the full editor.

(require "data/query.rkt"
         "data/syntax.rkt")

(provide
 ;; word scanning
 scan-word-forward scan-word-backward

 ;; symbol scanning
 scan-symbol-forward scan-symbol-backward

 ;; sexp scanning (balanced parens, strings, comments)
 scan-sexp-forward scan-sexp-backward

 ;; general char-class skip
 skip-char-class-forward skip-char-class-backward)

;; ============================================================
;; Internal: skip characters matching a predicate
;; ============================================================

(define (skip-while-forward gb pos len pred)
  (let loop ([p pos])
    (cond [(>= p len) len]
          [else
           (define-values (ch clen) (gap-char+len gb p))
           (if (pred ch) (loop (+ p clen)) p)])))

(define (skip-while-backward gb pos pred)
  (let loop ([p pos])
    (define prev (gap-prev-char-pos gb p))
    (cond [(<= prev 0)
           (if (and (= prev 0) (< 0 pos))
               (if (pred (gap-char gb 0)) 0 p)
               p)]
          [(pred (gap-char gb prev)) (loop prev)]
          [else p])))

;; ============================================================
;; Word scanning — Emacs-compatible forward-word / backward-word
;; ============================================================
;; forward-word: skip word-chars → skip whitespace → stop at next non-ws
;; backward-word: skip whitespace backward → skip word-chars backward

(define (scan-word-forward gb pos len st #:skip-ws? [skip-ws? #t])
  (define word? (λ (ch) (char-word? ch st)))
  (define after-word (skip-while-forward gb pos len word?))
  (if skip-ws?
      (skip-while-forward gb after-word len
        (λ (ch) (char-whitespace? ch st)))
      after-word))

(define (scan-word-backward gb pos st #:skip-ws? [skip-ws? #t])
  (define ws? (λ (ch) (char-whitespace? ch st)))
  (define word? (λ (ch) (char-word? ch st)))
  (define before-ws
    (if skip-ws?
        (skip-while-backward gb pos ws?)
        pos))
  (skip-while-backward gb before-ws word?))

;; ============================================================
;; Symbol scanning — like word but also includes symbol-constituent
;; ============================================================

(define (scan-symbol-forward gb pos len st)
  (define sym? (λ (ch) (or (char-word? ch st)
                           (char-symbol-constituent? ch st))))
  (skip-while-forward gb pos len sym?))

(define (scan-symbol-backward gb pos st)
  (define sym? (λ (ch) (or (char-word? ch st)
                           (char-symbol-constituent? ch st))))
  (skip-while-backward gb pos sym?))

;; ============================================================
;; General char-class skip
;; ============================================================

(define (skip-char-class-forward gb pos len st class)
  (define pred
    (case class
      [(word)       (λ (ch) (char-word? ch st))]
      [(whitespace) (λ (ch) (char-whitespace? ch st))]
      [(symbol)     (λ (ch) (char-symbol-constituent? ch st))]
      [(open)       (λ (ch) (char-open? ch st))]
      [(close)      (λ (ch) (char-close? ch st))]
      [(string-quote) (λ (ch) (char-string-quote? ch st))]
      [(punctuation)  (λ (ch) (char-punctuation? ch st))]
      [else (λ (_) #f)]))
  (skip-while-forward gb pos len pred))

(define (skip-char-class-backward gb pos st class)
  (define pred
    (case class
      [(word)       (λ (ch) (char-word? ch st))]
      [(whitespace) (λ (ch) (char-whitespace? ch st))]
      [(symbol)     (λ (ch) (char-symbol-constituent? ch st))]
      [(open)       (λ (ch) (char-open? ch st))]
      [(close)      (λ (ch) (char-close? ch st))]
      [(string-quote) (λ (ch) (char-string-quote? ch st))]
      [(punctuation)  (λ (ch) (char-punctuation? ch st))]
      [else (λ (_) #f)]))
  (skip-while-backward gb pos pred))

;; ============================================================
;; Sexp scanning — forward/backward over balanced expressions
;; ============================================================
;; Handles: open/close paren matching (nesting), strings, escaped chars,
;;          line comments, block comments (via multi-char rules).

(define (scan-sexp-forward gb pos len st)
  (cond [(>= pos len) len]
        [else
         (define ch (gap-char gb pos))
         (cond
           ;; Open delimiter — scan to matching close, tracking nesting
           [(char-open? ch st)
            (scan-list-forward gb pos len st ch)]
           ;; String quote — scan to closing quote
           [(or (char-string-quote? ch st) (char-string-delimiter? ch st))
            (scan-string-forward gb pos len st ch)]
           ;; Expression prefix — skip it, then scan the following expression
           [(char-expression-prefix? ch st)
            (define after-prefix (gap-next-char-pos gb pos))
            (scan-sexp-forward gb after-prefix len st)]
           ;; Comment start — skip entire comment
           [(char-comment-start? ch st)
            (scan-comment-forward gb pos len st)]
           ;; Default — skip one symbol/word
           [else
            (scan-symbol-forward gb pos len st)])]))

(define (scan-sexp-backward gb pos st)
  (cond [(<= pos 0) 0]
        [else
         (define prev (gap-prev-char-pos gb pos))
         (define ch (gap-char gb prev))
         (cond
           ;; Close delimiter — scan back to matching open
           [(char-close? ch st)
            (scan-list-backward gb pos st ch)]
           ;; String quote — scan back to opening quote
           [(or (char-string-quote? ch st) (char-string-delimiter? ch st))
            (scan-string-backward gb pos st ch)]
           ;; Comment end? Treat as atom
           [(char-comment-start? ch st)
            prev]
           ;; Default — skip one symbol/word backward
           [else
            (scan-symbol-backward gb pos st)])]))

;; ============================================================
;; matching-open / matching-close — paren pair lookup
;; ============================================================

(define (matching-close open-ch st)
  (case open-ch
    [(#\() #\)]
    [(#\[) #\]]
    [(#\{) #\}]
    [(#\<) #\>]
    [else  #\)]))

(define (matching-open close-ch st)
  (case close-ch
    [(#\)) #\(]
    [(#\]) #\[]
    [(#\}) #\{]
    [(#\>) #\<]
    [else  #\(]))

;; ============================================================
;; scan-list-forward/backward — balanced paren matching
;; ============================================================

(define (scan-list-forward gb start len st open-ch)
  ;; Find the matching close delimiter for `open-ch`.
  ;; Tracks nesting depth, skipping strings and comments properly.
  ;; Returns byte-pos right after the closing delimiter.
  (define close-ch (matching-close open-ch st))
  (let loop ([p (gap-next-char-pos gb start)] [depth 1])
    (cond [(>= p len) len]
          [else
           (define ch (gap-char gb p))
           (cond
             [(char=? ch open-ch)
              (loop (gap-next-char-pos gb p) (add1 depth))]
             [(char=? ch close-ch)
              (define new-depth (sub1 depth))
              (define next-p (gap-next-char-pos gb p))
              (if (zero? new-depth) next-p (loop next-p new-depth))]
             [(or (char-string-quote? ch st) (char-string-delimiter? ch st))
              (define after-str (scan-string-forward gb p len st ch))
              (loop after-str depth)]
             [(char-comment-start? ch st)
              (define after-cmt (scan-comment-forward gb p len st))
              (loop after-cmt depth)]
             [(char-escape? ch st)
              (loop (add1 (gap-next-char-pos gb p)) depth)]
             [else
              (loop (gap-next-char-pos gb p) depth)])])))

(define (scan-list-backward gb end st close-ch)
  ;; Find the matching open delimiter for `close-ch`, scanning backward.
  ;; Returns byte-pos of the opening delimiter.
  (define open-ch (matching-open close-ch st))
  (let loop ([p (gap-prev-char-pos gb end)] [depth 1])
    (cond [(<= p 0) 0]
          [else
           (define ch (gap-char gb p))
           (cond
             [(char=? ch close-ch)
              (loop (gap-prev-char-pos gb p) (add1 depth))]
             [(char=? ch open-ch)
              (define new-depth (sub1 depth))
              (if (zero? new-depth) p (loop (gap-prev-char-pos gb p) new-depth))]
             [(or (char-string-quote? ch st) (char-string-delimiter? ch st))
              (define before-str (scan-string-backward gb
                                                       (add1 (gap-next-char-pos gb p))
                                                       st ch))
              (loop before-str depth)]
             [else
              (loop (gap-prev-char-pos gb p) depth)])])))

;; ============================================================
;; scan-string-forward/backward — skip over a string literal
;; ============================================================

(define (scan-string-forward gb start len st quote-ch)
  (let loop ([p (gap-next-char-pos gb start)])
    (cond [(>= p len) len]
          [else
           (define ch (gap-char gb p))
           (cond
             [(char=? ch quote-ch)
              ;; Check it's not escaped
              (define prev (gap-prev-char-pos gb p))
              (if (and (> prev 0) (char=? (gap-char gb prev) #\\))
                  (loop (gap-next-char-pos gb p))
                  (gap-next-char-pos gb p))]       ;; past closing quote
             [(char-escape? ch st)
              (loop (add1 (gap-next-char-pos gb p)))] ;; skip escape + next
             [else
              (loop (gap-next-char-pos gb p))])])))

(define (scan-string-backward gb end st quote-ch)
  ;; end is the position AFTER the closing quote
  (let loop ([p (gap-prev-char-pos gb end)])
    (cond [(<= p 0) 0]
          [else
           (define ch (gap-char gb p))
           (cond
             [(char=? ch quote-ch)
              ;; Check not escaped
              (define prev (gap-prev-char-pos gb p))
              (if (and (> prev 0) (char=? (gap-char gb prev) #\\))
                  (loop prev)
                  p)]         ;; at the opening quote
             [else
              (loop (gap-prev-char-pos gb p))])])))

;; ============================================================
;; scan-comment-forward — skip a comment
;; ============================================================

(define (scan-comment-forward gb pos len st)
  ;; For line comments (like ; in Lisp): skip to end of line
  ;; For block comments (#|...|#): scan to matching close delimiter.
  ;; First check if there's a multi-char rule matching at pos.
  (define rules (syntax-table-multi-rules st))
  (define block-rule
    (for/or ([r (in-list rules)])
      (and (eq? (multi-char-rule-tag r) 'block-comment)
           (gap-match-str-at gb pos (multi-char-rule-start-str r))
           r)))
  (if block-rule
      (scan-block-comment-forward gb pos len
                                  (multi-char-rule-start-str block-rule)
                                  (multi-char-rule-end-str block-rule)
                                  (multi-char-rule-nestable? block-rule))
      ;; Line comment: scan to next newline
      (let loop ([p (gap-next-char-pos gb pos)])
        (cond [(>= p len) len]
              [(char=? (gap-char gb p) #\newline) (gap-next-char-pos gb p)]
              [else (loop (gap-next-char-pos gb p))]))))

(define (scan-block-comment-forward gb pos len start-str end-str nestable?)
  (define start-len (bytes-length (string->bytes/utf-8 start-str)))
  (let loop ([p (+ pos start-len)] [depth 1])
    (cond [(>= p len) len]
          [(zero? depth) p]
          [(gap-match-str-at gb p end-str)
           (loop (gap-skip-n gb p (string-length end-str)) (sub1 depth))]
          [(and nestable? (gap-match-str-at gb p start-str))
           (loop (gap-skip-n gb p (string-length start-str)) (add1 depth))]
          [else
           (loop (gap-next-char-pos gb p) depth)])))
