#lang racket

;; base/completion-algo.rkt — Completion algorithms on top of the protocol
;;
;; Provides matching, filtering, and default completion styles.
;; The protocol (source types, style struct) lives in kernel/completion.rkt.

(require "../kernel/completion.rkt")

(provide
 ;; core query
 completion-candidates
 completion-exact-match?

 ;; metadata
 completion-metadata

 ;; default completion styles
 prefix-completion-style
 substring-completion-style)

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
;; Default completion styles — pluggable matching strategies
;; ============================================================

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
