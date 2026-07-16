#lang racket

;; display/face.rkt — Face/color system
;;
;; face-attrs (logical) → realized-face (ANSI bytes) → face-cache.
;; Used by display/render.rkt for per-character face rendering.

(require "../platform/ansi.rkt")

(provide
 ;; attribute keys
 attr-foreground attr-background attr-weight attr-slant
 attr-underline attr-inverse-video

 ;; face-attrs
 make-face-attrs face-attrs? face-attrs-props

 ;; realized-face
 realized-face? realized-face-id realized-face-attrs
 realized-face-ansi-bytes

 ;; face cache
 make-face-cache face-cache? face-cache-by-id face-cache-next-id
 face-cache-lookup-or-realize!

 ;; global cache
 current-face-cache init-face-cache!)

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
;; face-attrs
;; ============================================================

(struct face-attrs (props) #:transparent)

(define (make-face-attrs . kvs)
  (face-attrs
   (for/hash ([i (in-range 0 (length kvs) 2)])
     (values (list-ref kvs i) (list-ref kvs (add1 i))))))

;; ============================================================
;; Realized face
;; ============================================================

(struct realized-face (id attrs ansi-bytes) #:transparent)

(define (realize-face id attrs depth)
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
  (realized-face id attrs (get-output-bytes out)))

;; ============================================================
;; Color helpers
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
;; Face cache
;; ============================================================

(struct face-cache
  ([table #:mutable] [by-id #:mutable] [next-id #:mutable])
  #:transparent)

(define (make-face-cache)
  (define default-rf (realize-face 0 (make-face-attrs) (color-depth)))
  (face-cache (make-hash) (vector default-rf) 1))

(define (face-cache-lookup-or-realize! fc attrs depth)
  (define key (face-attrs-props attrs))
  (hash-ref! (face-cache-table fc) key
    (λ ()
      (define id (face-cache-next-id fc))
      (define rf (realize-face id attrs depth))
      (set-face-cache-next-id! fc (add1 id))
      (set-face-cache-by-id! fc (vector-append (face-cache-by-id fc) (vector rf)))
      rf)))

;; ============================================================
;; Global face cache
;; ============================================================

(define global-face-cache (box #f))
(define (current-face-cache) (unbox global-face-cache))
(define (init-face-cache!)
  (unless (unbox global-face-cache)
    (set-box! global-face-cache (make-face-cache))))
