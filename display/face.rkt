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
 make-face-attrs face-attrs? face-attrs-props face-attrs-ref

 ;; realized-face
 realized-face? realized-face-id realized-face-attrs
 realized-face-ansi-bytes

 ;; face cache
 make-face-cache face-cache? face-cache-by-id face-cache-next-id
 face-cache-lookup-or-realize!

 ;; named faces
 define-face! face-id-for-name

 ;; face merging
 merge-face-attrs face-id-with-overlay

 ;; default face
 current-default-foreground current-default-background
 effective-default-attrs

 ;; predefined face names
 default-face region-face

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

(define (face-attrs-ref fa key [default #f])
  (hash-ref (face-attrs-props fa) key default))

;; ============================================================
;; Realized face
;; ============================================================

(struct realized-face (id attrs ansi-bytes) #:transparent)

(define (realize-face id attrs depth)
  (define out (open-output-bytes))
  (define fg (face-attrs-ref attrs attr-foreground #f))
  (when fg (display (color->ansi-fg fg depth) out))
  (define bg (face-attrs-ref attrs attr-background #f))
  (when bg (display (color->ansi-bg bg depth) out))
  (when (eq? (face-attrs-ref attrs attr-weight 'normal) 'bold)
    (display format-bold out))
  (when (eq? (face-attrs-ref attrs attr-slant 'normal) 'italic)
    (display format-italic out))
  (when (face-attrs-ref attrs attr-underline #f)
    (display format-underline out))
  (when (face-attrs-ref attrs attr-inverse-video #f)
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
;; Named faces — name → face-attrs → cache → face-id
;; ============================================================

(define named-face-table (make-hash))  ; symbol → face-attrs?

(define (define-face! name attrs)
  (hash-set! named-face-table name attrs))

(define (face-id-for-name name)
  (define fc (current-face-cache))
  (cond [(not fc) 0]
        [(hash-has-key? named-face-table name)
         (realized-face-id
          (face-cache-lookup-or-realize! fc
            (hash-ref named-face-table name)
            (color-depth)))]
        [else 0]))

(define (face-attrs-by-name name)
  (hash-ref named-face-table name (λ () (make-face-attrs))))

;; Merging: overlay face on top of base, non-#f values override
(define (merge-face-attrs base overlay)
  (define base-props (face-attrs-props base))
  (define overlay-props (face-attrs-props overlay))
  (define merged (make-hash))
  (for ([(k v) (in-hash base-props)]) (hash-set! merged k v))
  (for ([(k v) (in-hash overlay-props)] #:when v) (hash-set! merged k v))
  (face-attrs merged))

(define (face-id-with-overlay base-face-name overlay-face-name)
  (define fc (current-face-cache))
  (and fc
       (let* ([base-attrs (if base-face-name
                               (face-attrs-by-name base-face-name)
                               (effective-default-attrs))]
              [final-attrs (if overlay-face-name
                                (merge-face-attrs base-attrs (face-attrs-by-name overlay-face-name))
                                base-attrs)])
         (realized-face-id
          (face-cache-lookup-or-realize! fc final-attrs (color-depth))))))

;; Dynamic default colors
(define current-default-foreground (make-parameter #f))
(define current-default-background (make-parameter #f))

(define (effective-default-attrs)
  (define fg (current-default-foreground))
  (define bg (current-default-background))
  (if (or fg bg)
      (apply make-face-attrs
             (append (if fg (list attr-foreground fg) '())
                     (if bg (list attr-background bg) '())))
      (face-attrs-by-name default-face)))

;; Predefined faces
(define default-face 'default)
(define region-face 'region)

(define-face! default-face (make-face-attrs))
(define-face! region-face (make-face-attrs attr-background 8))

;; ============================================================
;; Global face cache
;; ============================================================

(define global-face-cache (box #f))
(define (current-face-cache) (unbox global-face-cache))
(define (init-face-cache!)
  (unless (unbox global-face-cache)
    (set-box! global-face-cache (make-face-cache))))
