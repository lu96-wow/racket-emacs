#lang racket

;; kernel/completion.rkt — Completion protocol (zero UI, zero display, zero algorithm)
;;
;; A completion-source is one of:
;;   · (listof string?)           — static candidate list
;;   · (hash/c string? any/c)     — hash (key=candidate, value=metadata)
;;   · (-> string? (listof string?)) — function: prefix → matching candidates
;;
;; This module defines ONLY the protocol: source types, style struct,
;; and the active-styles parameter.  Matching algorithms are in
;; base/completion-algo.rkt.

(provide
 ;; protocol
 completion-source?
 completion-source-list? completion-source-hash? completion-source-proc?

 ;; completion styles (protocol)
 completion-style? make-completion-style
 completion-style-name completion-style-match-fn completion-style-highlight-fn
 completion-styles)

;; ============================================================
;; Completion source protocol
;; ============================================================

(define (completion-source-list? v) (and (list? v) (andmap string? v)))
(define (completion-source-hash? v) (hash? v))
(define (completion-source-proc? v) (procedure? v))

(define (completion-source? v)
  (or (completion-source-list? v)
      (completion-source-hash? v)
      (completion-source-proc? v)))

;; ============================================================
;; Completion styles — pluggable matching strategies
;; ============================================================

(struct completion-style
  (name        ; symbol?
   match-fn    ; string? string? string? → (or/c #f (listof (cons/c int? int?)))
               ;   match-fn prefix candidate full-candidate → match-spans or #f
   highlight-fn) ; string? string? string? → string?
               ;   highlight-fn prefix candidate full-candidate → highlighted-string
  #:transparent)

(define (make-completion-style name match-fn highlight-fn)
  (completion-style name match-fn highlight-fn))

;; Active completion styles (parameter, so frontends can customize)
(define completion-styles
  (make-parameter '()))
