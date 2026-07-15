#lang racket

;; user/racket-doc.rkt — Racket documentation lookup via blueboxes
;;
;; C-c C-d: look up the identifier at point, display signature in bottom line.
;; The doc panel stays visible until the next keypress.

(require setup/xref
         scribble/xref
         scribble/blueboxes
         scribble/manual-struct
         "../kernel/buffer.rkt"
         "../kernel/bottom-input.rkt"
         "../base/edit.rkt"
         racket/list
         racket/string)

(provide racket-doc-lookup)

;; ============================================================
;; Lazy xref loader (expensive first time, ~3-5s)
;; ============================================================

(define xref-cache (box #f))

(define (get-xref)
  (unless (unbox xref-cache)
    (set-box! xref-cache (load-collections-xref void)))
  (unbox xref-cache))

;; ============================================================
;; Module priority — higher score = more relevant for lookups
;; ============================================================

(define (module-priority mp)
  ;; Resolve module-path to a comparable key.
  ;; Lower number = higher priority.
  (define str
    (cond [(symbol? mp) (symbol->string mp)]
          [(pair? mp)   (format "~a" mp)]
          [(path? mp)   (path->string mp)]
          [else         (format "~a" mp)]))
  (cond [(string-prefix? str "racket/base")  0]
        [(string-prefix? str "racket/list")  1]
        [(string-prefix? str "racket/")      2]
        [(string-prefix? str "racket")       3]
        [(string-prefix? str "srfi/")        4]
        [else                                5]))

;; ============================================================
;; Search xref index for a symbol, return prioritized list of tags
;; ============================================================

(define (find-tags-for-symbol sym)
  (define xref (get-xref))
  (define idx (xref-index xref))
  (define candidates '())
  (for ([e (in-list idx)])
    (define desc (entry-desc e))
    (when (exported-index-desc*? desc)
      (when (eq? sym (exported-index-desc-name desc))
        (define libs (exported-index-desc-from-libs desc))
        (define tag  (entry-tag e))
        (define prio (if (null? libs) 99
                         (apply min (map module-priority libs))))
        (set! candidates
              (cons (cons prio tag) candidates)))))
  ;; Sort by priority, return tags
  (map cdr (sort candidates < #:key car)))

;; ============================================================
;; Format bluebox strings into readable help text
;; ============================================================

(define (format-bluebox strs)
  ;; strs is (kind signature contracts ...) — possibly multiple overloads
  ;; Returns a list of display lines.
  (define lines '())
  (define (emit fmt . args) (set! lines (cons (apply format fmt args) lines)))

  (define kind #f)
  (for ([s (in-list strs)])
    (define s-trim (string-trim s))
    (cond [(member s-trim '("procedure" "syntax" "function" "method"
                            "struct" "class" "interface" "mixin"))
           (set! kind s-trim)]
          [(string-prefix? s-trim "(")
           (when kind (emit "~a: ~a" kind s-trim) (set! kind #f))]
          [(and kind (not (equal? s-trim "")))
           (emit "  ~a" s-trim)]
          [(equal? s-trim "")
           (emit "")]
          [else (void)]))

  (reverse lines))

;; ============================================================
;; Main command: racket-doc-lookup
;; ============================================================

(define (racket-doc-lookup)
  (define buf (current-buffer))

  (let/ec return
    ;; 1. Get the symbol at point
    (define sym-str (symbol-at-point #:buf buf))
    (unless sym-str
      (bottom-line-set-echo! "No identifier at point")
      (return))

    ;; 2. Search xref index
    (define sym (string->symbol sym-str))
    (define tags
      (with-handlers ([exn:fail? (λ (e)
                                   (bottom-line-set-echo!
                                    (format "Error loading docs index: ~a" (exn-message e)))
                                   (return))])
        (find-tags-for-symbol sym)))
    (when (null? tags)
      (bottom-line-set-echo!
       (format "No documentation found for `~a'" sym-str))
      (return))

    ;; 3. Fetch bluebox strings
    (define best-tag (first tags))
    (define bluebox-strs
      (with-handlers ([exn:fail? (λ (e)
                                   (bottom-line-set-echo!
                                    (format "Error fetching docs for `~a': ~a"
                                            sym-str (exn-message e)))
                                   (return))])
        (fetch-blueboxes-strs best-tag)))

    ;; 4. Format and display in bottom line
    (define formatted (format-bluebox bluebox-strs))
    (define header (format "─ ~a ─" sym-str))
    (bottom-line-set-doc! (cons header formatted))))
