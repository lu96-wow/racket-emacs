#lang racket

;; display/face.rkt — Minimal face system
;;
;; face-attrs (logical properties) → realized-face (ANSI bytes).
;; Simple cache keyed by props hash.
;; Depends only on platform/ansi.rkt for color constants.

(require "../platform/ansi.rkt")

(provide
 ;; face-attrs
 make-face-attrs face-attrs? face-attrs-props
 attr-foreground attr-background attr-weight attr-slant
 attr-underline attr-inverse-video

 ;; realize
 realize-face

 ;; cache
 make-face-cache face-cache-lookup!)

;; ============================================================
;; Attribute keys
;; ============================================================

(define attr-foreground   'foreground)
(define attr-background   'background)
(define attr-weight       'weight)
(define attr-slant        'slant)
(define attr-underline    'underline)
(define attr-inverse-video 'inverse-video)

;; ============================================================
;; face-attrs — logical face description
;; ============================================================

(struct face-attrs (props) #:transparent)

(define (make-face-attrs . kvs)
  (face-attrs
   (for/hash ([i (in-range 0 (length kvs) 2)])
     (values (list-ref kvs i) (list-ref kvs (add1 i))))))

;; ============================================================
;; realize-face — face-attrs + depth → ANSI bytes string
;; ============================================================

(define (realize-face attrs depth)
  (define out (open-output-bytes))
  (define fg (hash-ref (face-attrs-props attrs) attr-foreground #f))
  (when fg (display (color->ansi-fg fg depth) out))
  (define bg (hash-ref (face-attrs-props attrs) attr-background #f))
  (when bg (display (color->ansi-bg bg depth) out))
  (when (eq? (hash-ref (face-attrs-props attrs) attr-weight 'normal) 'bold)
    (display format-bold out))
  (when (eq? (hash-ref (face-attrs-props attrs) attr-slant 'normal) 'italic)
    (display format-italic out))
  (when (hash-ref (face-attrs-props attrs) attr-underline #f)
    (display format-underline out))
  (when (hash-ref (face-attrs-props attrs) attr-inverse-video #f)
    (display format-reverse out))
  (get-output-bytes out))

;; ============================================================
;; Color → ANSI helpers
;; ============================================================

(define (color->ansi-fg c depth)
  (match c
    [(? exact-nonnegative-integer? n)
     (case depth [(truecolor) (ansi-fg-256 n)] [(256) (ansi-fg-256 n)]
                 [(16) (ansi-fg-16 (quotient n 16))] [else ""])]
    [(list r g b)
     (case depth [(truecolor) (ansi-fg r g b)] [(256) (ansi-fg-256 (rgb->256 r g b))]
                 [(16) (ansi-fg-16 (rgb->16 r g b))] [else ""])]
    [_ ""]))

(define (color->ansi-bg c depth)
  (match c
    [(? exact-nonnegative-integer? n)
     (case depth [(truecolor) (ansi-bg-256 n)] [(256) (ansi-bg-256 n)]
                 [(16) (ansi-bg-16 (quotient n 16))] [else ""])]
    [(list r g b)
     (case depth [(truecolor) (ansi-bg r g b)] [(256) (ansi-bg-256 (rgb->256 r g b))]
                 [(16) (ansi-bg-16 (rgb->16 r g b))] [else ""])]
    [_ ""]))

(define (rgb->256 r g b)
  (if (= r g b)
      (if (< r 8) 16 (+ 232 (quotient (- r 8) 10)))
      (+ 16 (* 36 (quotient r 51)) (* 6 (quotient g 51)) (quotient b 51))))

(define (rgb->16 r g b)
  (define bright? (> (+ r g b) (* 3 128)))
  (+ (if bright? 8 0) (if (> r 128) 1 0) (if (> g 128) 2 0) (if (> b 128) 4 0)))

;; ============================================================
;; Face cache — props hash → ansi-bytes
;; ============================================================

(struct face-cache
  ([table #:mutable])  ; (hash/c hash? bytes?)
  #:transparent)

(define (make-face-cache)
  (define default-bs (realize-face (make-face-attrs) (color-depth)))
  (define tbl (make-hash))
  (hash-set! tbl (hasheq) default-bs)
  (face-cache tbl))

(define (face-cache-lookup! fc attrs)
  (define key (face-attrs-props attrs))
  (hash-ref! (face-cache-table fc) key
    (λ () (realize-face attrs (color-depth)))))
