#lang racket

;; kernel/completion.rkt — Completion protocol (zero UI, zero display)
;;
;; A completion-source is one of:
;;   · (listof string?)           — static candidate list
;;   · (hash/c string? any/c)     — hash (key=candidate, value=metadata)
;;   · (-> string? (listof string?)) — function: prefix → matching candidates
;;
;; The ONLY query function is completion-candidates.  All UI logic
;; (sorting, highlighting, paging) lives in upper layers.

(provide
 ;; protocol
 completion-source?
 completion-source-list? completion-source-hash? completion-source-proc?

 ;; core query
 completion-candidates
 completion-exact-match?

 ;; metadata
 completion-metadata

 ;; completion styles
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
;; completion-candidates — the single query entry point
;; ============================================================
;;
;; Returns all candidates from `source` that match `prefix`
;; (case-insensitive prefix match).  Results are unsorted.

(define (completion-candidates source prefix)
  (define prefix-ci (string-downcase prefix))
  (define (prefix-match? s)
    (let ([s-ci (string-downcase s)])
      (and (>= (string-length s-ci) (string-length prefix-ci))
           (string-prefix? s-ci prefix-ci))))
  (cond
    [(completion-source-list? source)
     (filter prefix-match? source)]
    [(completion-source-hash? source)
     (filter prefix-match? (hash-keys source))]
    [(completion-source-proc? source)
     (source prefix)]
    [else '()]))

;; ============================================================
;; completion-exact-match?
;; ============================================================

(define (completion-exact-match? source str)
  (cond
    [(completion-source-list? source) (member str source)]
    [(completion-source-hash? source) (hash-has-key? source str)]
    [(completion-source-proc? source) (not (null? (source str)))]
    [else #f]))

;; ============================================================
;; Metadata — extensible without changing the protocol
;; ============================================================

(define (completion-metadata source)
  ;; Sources can attach metadata via:
  ;;   - hash values: (hash "candidate" '(:annotation "doc" :category 'function))
  ;;   - property on the source itself (not yet implemented)
  ;; Returns (hash/c symbol? any/c), always succeeds.
  (make-hash))

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

;; Default styles
(define prefix-completion-style
  (make-completion-style
   'prefix
   ;; match-fn: case-insensitive prefix match
   (λ (prefix candidate _full)
     (define ci-prefix (string-downcase prefix))
     (define ci-cand (string-downcase candidate))
     (if (string-prefix? ci-cand ci-prefix)
         (list (cons 0 (string-length prefix)))
         #f))
   ;; highlight-fn: bold the prefix portion
   (λ (prefix candidate _full)
     (define plen (string-length prefix))
     (if (and (positive? plen)
              (<= plen (string-length candidate)))
         (string-append "\e[1m" (substring candidate 0 plen) "\e[0m"
                        (substring candidate plen))
         candidate))))

(define substring-completion-style
  (make-completion-style
   'substring
   (λ (prefix candidate _full)
     ;; Find prefix as substring (case-insensitive), return its span
     (define ci-prefix (string-downcase prefix))
     (define ci-cand (string-downcase candidate))
     (define idx (string-contains? ci-cand ci-prefix))
     (and idx (list (cons idx (+ idx (string-length prefix))))))
   (λ (prefix candidate _full)
     (define ci-prefix (string-downcase prefix))
     (define ci-cand (string-downcase candidate))
     (define idx (string-contains? ci-cand ci-prefix))
     (if idx
         (string-append (substring candidate 0 idx)
                        "\e[1m"
                        (substring candidate idx (+ idx (string-length prefix)))
                        "\e[0m"
                        (substring candidate (+ idx (string-length prefix))))
         candidate))))

;; Active completion styles (parameter, so frontends can customize)
(define completion-styles
  (make-parameter (list prefix-completion-style)))
