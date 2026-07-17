#lang racket

;; lang/define.rkt — Language definition (pure data)
;;
;; A lang-def is a bundle of everything a language needs for
;; fontification.  It's pure data — no behaviour, no registry,
;; no side effects.
;;
;; The caller (main.rkt) is responsible for:
;;   - collecting lang-defs into a pattern→lang-def table
;;   - matching buffer filename → selecting the right lang-def
;;   - calling activate-language! to register faces
;;   - calling lang-def->font-lock-config to build the config
;;   - calling fontify-region! to apply

(require "syntax.rkt"
         "font-lock.rkt"
         "../display/face.rkt")

(provide
 ;; lang-def — pure data
 lang-def? lang-def
 lang-def-name lang-def-patterns
 lang-def-syntax-table lang-def-keywords lang-def-faces

 ;; pure conversions
 lang-def->syntax-config  ; lang-def → syntax-config

 ;; application (imperative — registers faces into global face-cache)
 activate-language!)         ; lang-def → void

;; ============================================================
;; lang-def — pure data, zero behaviour
;; ============================================================

(struct lang-def
  (name           ; symbol — 'racket, 'python, 'c, ...
   patterns        ; (listof string?) — filename patterns (".rkt" ".py")
   syntax-table   ; syntax-table? | #f
   keywords        ; (listof (cons/c pregexp? symbol?))
   faces           ; (listof (list/c symbol? face-attrs?))
   )
  #:transparent)

;; ============================================================
;; lang-def->syntax-config — pure
;; ============================================================

(define (lang-def->syntax-config ld)
  (make-syntax-config
   #:syntax-table (lang-def-syntax-table ld)
   #:keywords     (lang-def-keywords ld)
   #:case-fold?   #f))

;; ============================================================
;; activate-language! — register faces (imperative, writes to face-cache)
;; ============================================================

(define (activate-language! ld)
  (for ([f (in-list (lang-def-faces ld))])
    (match-define (list name attrs) f)
    (define-face! name attrs)))
