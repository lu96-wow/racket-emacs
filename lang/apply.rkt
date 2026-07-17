#lang racket

;; lang/apply.rkt — Syntax highlighting application layer
;;
;; Unified entry point.  Hides language matching, face registration,
;; and per-buffer config storage.  The caller (main.rkt) sees:
;;
;;   (syntax-setup! buf)         — match → activate → store config → scan all
;;   (syntax-update! buf extent) — incremental scan after edit
;;   (syntax-highlight-buffer! buf)  — full re-scan
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
 syntax-setup!
 syntax-update!
 syntax-highlight-buffer!

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

(define (buffer-syntax-config buf)
  (hash-ref config-table buf (λ () #f)))

(define (set-buffer-syntax-config! buf cfg)
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

(define (syntax-setup! buf)
  ;; 1. Match language by filename
  (define ld (or (match-language buf) (default-language)))
  ;; 2. Register faces in global face-cache
  (activate-language! ld)
  ;; 3. Build syntax config and store on buffer
  (define cfg (lang-def->syntax-config ld))
  (set-buffer-syntax-config! buf cfg)
  ;; 4. Scan entire buffer
  (syntax-highlight-buffer! buf))

;; ============================================================
;; syntax-highlight-buffer! — full scan
;; ============================================================

(define (syntax-highlight-buffer! buf)
  (define cfg (buffer-syntax-config buf))
  (unless cfg
    (error 'syntax-highlight-buffer! "no syntax config for buffer ~a"
           (buffer-name buf)))
  (define buflen (buffer-length buf))
  (when (positive? buflen)
    (define gb (text-gap (buffer-text buf)))
    (define tp (buffer-text-props buf))
    (textprop-remove! tp 0 buflen)
    (syntax-highlight-region! gb tp cfg 0 buflen)))

;; ============================================================
;; syntax-update! — incremental scan after edit
;; ============================================================

(define (syntax-update! buf extent)
  (define cfg (buffer-syntax-config buf))
  (unless cfg (void))
  (define gb (text-gap (buffer-text buf)))
  (define tp (buffer-text-props buf))
  (syntax-highlight-changed! gb tp cfg extent))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)

  ;; Populate available languages for tests
  (set-box! available-languages (list racket-lang-def))

  (init-face-cache!)

  (test-case "syntax-setup! on scratch buffer"
    (define buf (make-buffer "*scratch*" ";; comment\n(define x 1)\n"))
    (syntax-setup! buf)
    (check-true (buffer-syntax-config buf) "should have config")
    (check-equal? (buffer-face-at buf 0) 'font-lock-comment-face)
    (check-equal? (buffer-face-at buf 13) 'font-lock-keyword-face))

  (test-case "syntax-update! after edit"
    (define buf (make-buffer "*test*" "(define a 1)\n(define b 2)\n"))
    (syntax-setup! buf)
    (buffer-insert! buf "xxx" 5)
    (define ext (cons 5 8))
    (syntax-update! buf ext)
    (check-equal? (buffer-face-at buf 2) 'font-lock-keyword-face))

  (test-case "match-language — .rkt file"
    (define buf (make-buffer "foo.rkt" "#lang racket\n"))
    (set-buffer-filename! buf "/home/user/foo.rkt")
    (define ld (match-language buf))
    (check-true ld)
    (check-eq? (lang-def-name ld) 'racket)))
