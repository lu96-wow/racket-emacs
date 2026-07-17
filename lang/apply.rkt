#lang racket

;; lang/apply.rkt — Syntax highlighting application layer
;;
;; Zero module-level state.  The caller owns the config table and
;; language list, passing them as explicit arguments — same pattern
;; as input/keymap.rkt's keymap-resolve.
;;
;;   (syntax-setup!    table buf languages) — match → activate → store → scan
;;   (syntax-update!   table buf extent)    — incremental scan after edit
;;   (syntax-highlight-buffer! table buf)   — full re-scan
;;   (syntax-config-get table buf)          — read per-buffer config
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
 ;; operations (table + languages passed explicitly by caller)
 syntax-config-get
 syntax-config-set!
 syntax-setup!
 syntax-update!
 syntax-highlight-buffer!

 ;; pure matching
 match-language)

;; ============================================================
;; Per-buffer config access — caller owns the table
;; ============================================================

(define (syntax-config-get table buf)
  ;; table : (hash/c buffer? syntax-config?) — caller-owned
  (hash-ref table buf (λ () #f)))

(define (syntax-config-set! table buf cfg)
  (hash-set! table buf cfg))

;; ============================================================
;; Matching — filename → lang-def (pure)
;; ============================================================

(define (match-language buf languages)
  ;; buf       : buffer?
  ;; languages : (listof lang-def?) — caller-owned, no box needed
  (define fname (buffer-filename buf))
  (and fname
       (for/or ([ld (in-list languages)])
         (for/or ([pat (in-list (lang-def-patterns ld))])
           (and (string-contains? fname pat) ld)))))

(define (default-language) racket-lang-def)

;; ============================================================
;; fontify-setup! — match → activate → store → fontify
;; ============================================================

(define (syntax-setup! table buf languages)
  ;; table     : (hash/c buffer? syntax-config?) — caller-owned
  ;; buf       : buffer?
  ;; languages : (listof lang-def?)
  ;; 1. Match language by filename
  (define ld (or (match-language buf languages) (default-language)))
  ;; 2. Register faces in global face-cache
  (activate-language! ld)
  ;; 3. Build syntax config and store on buffer
  (define cfg (lang-def->syntax-config ld))
  (syntax-config-set! table buf cfg)
  ;; 4. Scan entire buffer
  (syntax-highlight-buffer! table buf))

;; ============================================================
;; syntax-highlight-buffer! — full scan
;; ============================================================

(define (syntax-highlight-buffer! table buf)
  (define cfg (syntax-config-get table buf))
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

(define (syntax-update! table buf extent)
  (define cfg (syntax-config-get table buf))
  (unless cfg (void))
  (define gb (text-gap (buffer-text buf)))
  (define tp (buffer-text-props buf))
  (syntax-highlight-changed! gb tp cfg extent))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)

  (init-face-cache!)

  (test-case "syntax-setup! on scratch buffer"
    (define table (make-hasheq))
    (define languages (list racket-lang-def))
    (define buf (make-buffer "*scratch*" ";; comment\n(define x 1)\n"))
    (syntax-setup! table buf languages)
    (check-pred syntax-config? (syntax-config-get table buf) "should have config")
    (check-equal? (buffer-face-at buf 0) 'font-lock-comment-face)
    (check-equal? (buffer-face-at buf 13) 'font-lock-keyword-face))

  (test-case "syntax-update! after edit"
    (define table (make-hasheq))
    (define languages (list racket-lang-def))
    (define buf (make-buffer "*test*" "(define a 1)\n(define b 2)\n"))
    (syntax-setup! table buf languages)
    (buffer-insert! buf "xxx" 5)
    (define ext (cons 5 8))
    (syntax-update! table buf ext)
    (check-equal? (buffer-face-at buf 2) 'font-lock-keyword-face))

  (test-case "match-language — .rkt file"
    (define buf (make-buffer "foo.rkt" "#lang racket\n"))
    (set-buffer-filename! buf "/home/user/foo.rkt")
    (define ld (match-language buf (list racket-lang-def)))
    (check-pred lang-def? ld)
    (check-eq? (lang-def-name ld) 'racket)))
