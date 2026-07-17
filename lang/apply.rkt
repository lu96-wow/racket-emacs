#lang racket

;; lang/apply.rkt — Font-lock application layer
;;
;; Unified entry point for syntax highlighting.  Hides the details of
;; language matching, face registration, and per-buffer config storage.
;; The caller (main.rkt) sees three simple operations:
;;
;;   (fontify-setup! buf)         — match → activate → store config → fontify all
;;   (fontify-change! buf extent) — incremental fontify after edit
;;   (fontify-buffer! buf)        — full re-fontify
;;
;; Dependencies: kernel/buffer, lang/font-lock, lang/define, display/face

(require "../kernel/buffer.rkt"
         "../kernel/data/text.rkt"
         "../kernel/data/gap.rkt"
         "../kernel/data/textprop.rkt"
         "../display/face.rkt"
         "font-lock.rkt"
         "define.rkt"
         "racket-lang.rkt")  ; for default-language

(provide
 ;; operations (caller sees only these)
 fontify-setup!
 fontify-change!
 fontify-buffer!

 ;; language list (caller populates this)
 available-languages)

;; ============================================================
;; Language registry — a plain list, populated by caller
;; ============================================================

(define available-languages (box '()))

;; ============================================================
;; Per-buffer config storage
;; ============================================================

(define config-table (make-hasheq))

(define (buffer-fl-config buf)
  (hash-ref config-table buf (λ () #f)))

(define (set-buffer-fl-config! buf cfg)
  (hash-set! config-table buf cfg))

;; ============================================================
;; Matching — filename → lang-def
;; ============================================================

(define (match-language buf)
  (define fname (buffer-filename buf))
  (and fname
       (for/or ([ld (in-list (unbox available-languages))])
         (for/or ([pat (in-list (lang-def-patterns ld))])
           (and (string-contains? fname pat) ld)))))

(define (default-language) racket-lang-def)

;; ============================================================
;; fontify-setup! — match → activate → store → fontify
;; ============================================================

(define (fontify-setup! buf)
  ;; 1. Match language by filename
  (define ld (or (match-language buf) (default-language)))
  ;; 2. Register faces in global face-cache
  (activate-language! ld)
  ;; 3. Build font-lock config and store on buffer
  (define fl-cfg (lang-def->font-lock-config ld))
  (set-buffer-fl-config! buf fl-cfg)
  ;; 4. Fontify entire buffer
  (fontify-buffer! buf))

;; ============================================================
;; fontify-buffer! — full fontification
;; ============================================================

(define (fontify-buffer! buf)
  (define fl-cfg (buffer-fl-config buf))
  (unless fl-cfg
    (error 'fontify-buffer! "no font-lock config for buffer ~a"
           (buffer-name buf)))
  (define buflen (buffer-length buf))
  (when (positive? buflen)
    (define gb (text-gap (buffer-text buf)))
    (define tp (buffer-text-props buf))
    ;; Clear + fontify entire buffer
    (textprop-remove! tp 0 buflen)
    (fontify-region! gb tp fl-cfg 0 buflen)))

;; ============================================================
;; fontify-change! — incremental fontify after edit
;; ============================================================

(define (fontify-change! buf extent)
  ;; extent is (cons start end) from dirty-buffer
  (define fl-cfg (buffer-fl-config buf))
  (unless fl-cfg (void))
  (define gb (text-gap (buffer-text buf)))
  (define tp (buffer-text-props buf))
  (fontify-changed! gb tp fl-cfg extent))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)

  ;; Populate available languages for tests
  (set-box! available-languages (list racket-lang-def))

  (init-face-cache!)

  (test-case "fontify-setup! on scratch buffer"
    (define buf (make-buffer "*scratch*" ";; comment\n(define x 1)\n"))
    (fontify-setup! buf)
    (check-true (buffer-fl-config buf) "should have config")
    (check-equal? (buffer-face-at buf 0) 'font-lock-comment-face)
    (check-equal? (buffer-face-at buf 13) 'font-lock-keyword-face))

  (test-case "fontify-change! after edit"
    (define buf (make-buffer "*test*" "(define a 1)\n(define b 2)\n"))
    (fontify-setup! buf)
    (buffer-insert! buf "xxx" 5)
    (define ext (cons 5 8))
    (fontify-change! buf ext)
    ;; After insert, original 'define' keyword face should be preserved
    (check-equal? (buffer-face-at buf 2) 'font-lock-keyword-face))

  (test-case "match-language — .rkt file"
    (define buf (make-buffer "foo.rkt" "#lang racket\n"))
    (set-buffer-filename! buf "/home/user/foo.rkt")
    (define ld (match-language buf))
    (check-true ld)
    (check-eq? (lang-def-name ld) 'racket)))
