#lang racket

;; user/racket-doc.rkt — Racket documentation lookup via blueboxes
;;
;; C-c C-d (or F1): look up the identifier at point in Racket documentation.
;; Displays signature + contract in a *Help* buffer.

(require setup/xref
         scribble/xref
         scribble/blueboxes
         scribble/manual-struct
         "../kernel/buffer.rkt"
         "../kernel/window.rkt"
         "../kernel/bottom-input.rkt"
         "../base/edit.rkt"
         "../base/registry.rkt"
         "../base/window-ops.rkt"
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
  ;; We format as:  kind : signature\n    contracts...
  (define lines '())
  (define (emit fmt . args) (set! lines (cons (apply format fmt args) lines)))

  (define kind #f)
  (for ([s (in-list strs)])
    (define s-trim (string-trim s))
    (cond [(member s-trim '("procedure" "syntax" "function" "method"
                            "struct" "class" "interface" "mixin"))
           (set! kind s-trim)]
          [(string-prefix? s-trim "(")
           ;; Signature line
           (when kind (emit "~a:  ~a" kind s-trim) (set! kind #f))]
          [(and kind (not (equal? s-trim "")))
           ;; Contract lines after signature
           (emit "      ~a" s-trim)]
          [(equal? s-trim "")
           ;; Empty line = separator between overloads
           (emit "")]
          [else (void)]))

  (reverse lines))

;; ============================================================
;; Create or re-use a *Help* buffer
;; ============================================================

(define help-buffer-name "*Help*")

(define (get-help-buffer)
  (define existing (get-buffer help-buffer-name))
  (if existing
      (begin
        (buffer-delete existing 0 (buffer-byte-length existing))
        (set-buffer-read-only?! existing #f)
        existing)
      (let ([buf (get-buffer-create help-buffer-name)])
        (set-buffer-read-only?! buf #f)
        buf)))

(define (display-help-buffer help-buf sym-str lines)
  ;; Insert content into help buffer
  (define header (format "Racket documentation for `~a':\n\n" sym-str))
  (define body   (if (null? lines)
                     (format "No documentation found for `~a'.\n" sym-str)
                     (string-join lines "\n")))
  (define footer "\n\n── End of help ──\n")
  (define text (string-append header body footer))
  (buffer-insert help-buf text #:at 0)
  (set-buffer-point! help-buf 0)
  (set-buffer-read-only?! help-buf #t)
  (set-buffer-modified?! help-buf #f)

  ;; Display: if single window, split; otherwise find or use a non-selected window
  (define frm (current-frame))
  (define all-leaves (filter (λ (w) (and (window-leaf? w) (not (window-mini? w))))
                             (frame-window-list frm)))
  (define sel (selected-window))

  (cond
    [(= (length all-leaves) 1)
     ;; Single window: split below, show help in the new window,
     ;; keep original buffer focused.
     (split-window-below)
     (switch-buffer-in-window! (selected-window) help-buf)
     (other-window)]
    [else
     ;; Multiple windows: find a non-selected window that is NOT already
     ;; showing the help buffer, and switch it.
     (define other-win
       (findf (λ (w) (and (not (eq? w sel))
                          (not (eq? (window-buffer w) help-buf))))
              all-leaves))
     (when other-win
       (switch-buffer-in-window! other-win help-buf))]))

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

    ;; 4. Format and display
    (define formatted (format-bluebox bluebox-strs))
    (define help-buf (get-help-buffer))
    (display-help-buffer help-buf sym-str formatted)
    (bottom-line-clear-echo!)))
