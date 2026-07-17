#lang racket

;; lang/apply.rkt — Syntax highlighting application layer
;;
;; Zero module-level state.  Functions take syntax-config explicitly
;; — no table, no indirect lookup.  Caller stores config however it
;; wants (struct field, hash, parameter, ...).
;;
;;   (syntax-setup! buf languages)         → syntax-config
;;   (syntax-highlight-buffer! cfg buf)     → void
;;   (syntax-update! cfg buf extent)        → void
;;   (match-language buf languages)         → lang-def | #f
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
 syntax-setup!              ;; buf languages → syntax-config
 syntax-highlight-buffer!    ;; cfg buf → void
 syntax-update!              ;; cfg buf extent → void

 ;; pure matching
 match-language)             ;; buf languages → lang-def | #f

;; ============================================================
;; Matching — filename → lang-def (pure)
;; ============================================================

(define (match-language buf languages)
  ;; buf       : buffer?
  ;; languages : (listof lang-def?)
  (define fname (buffer-filename buf))
  (and fname
       (for/or ([ld (in-list languages)])
         (for/or ([pat (in-list (lang-def-patterns ld))])
           (and (string-contains? fname pat) ld)))))

(define (default-language) racket-lang-def)

;; ============================================================
;; syntax-setup! — match → activate → build → scan
;; ============================================================

(define (syntax-setup! buf languages)
  ;; buf       : buffer?
  ;; languages : (listof lang-def?)
  ;; → syntax-config? — caller stores it (e.g. in editor-buffer struct)
  ;; Side effects: registers faces in global cache, writes text-props on buf.
  (define ld (or (match-language buf languages) (default-language)))
  (activate-language! ld)
  (define cfg (lang-def->syntax-config ld))
  (syntax-highlight-buffer! cfg buf)
  cfg)

;; ============================================================
;; syntax-highlight-buffer! — full scan
;; ============================================================

(define (syntax-highlight-buffer! cfg buf)
  ;; cfg : syntax-config? — provided by caller (from editor-buffer field)
  ;; buf : buffer?
  (unless cfg
    (error 'syntax-highlight-buffer! "no syntax config for buffer ~a"
           (buffer-name buf)))
  (define buflen (buffer-length buf))
  (when (positive? buflen)
    (define gb (text-gap (buffer-text buf)))
    (define tp (buffer-text-props buf))
    (textprop-remove-key! tp 0 buflen 'face)
    (syntax-highlight-region! gb tp cfg 0 buflen)))

;; ============================================================
;; syntax-update! — incremental scan after edit
;; ============================================================

(define (syntax-update! cfg buf extent)
  ;; cfg    : syntax-config? | #f — #f means no config, skip
  ;; buf    : buffer?
  ;; extent : (cons/c exact-nonnegative-integer? exact-nonnegative-integer?)
  (unless cfg (void))
  (define gb (text-gap (buffer-text buf)))
  (define tp (buffer-text-props buf))
  (syntax-highlight-changed! gb tp cfg extent))
