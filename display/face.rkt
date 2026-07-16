#lang racket

;; display/face.rkt — Face system: logical attributes → face-id → ANSI
;;
;; Three layers:
;;   1. face-attrs  — logical properties (color, weight, slant)
;;   2. face-cache  — maps attrs → integer face-id, caches ANSI bytes
;;   3. named faces — symbol → face-attrs (keyword, comment, string, ...)
;;
;; The renderer sets cell.face-id from buffer text properties.
;; The flusher emits ANSI when face-id changes between cells.
;;
;; Dependencies: platform/ansi.rkt (format strings only, no state)

(require "../platform/ansi.rkt")

(provide
 ;; attribute keys
 attr-foreground attr-background attr-weight attr-slant
 attr-underline attr-inverse-video

 ;; face-attrs
 face-attrs? make-face-attrs face-attrs-ref face-attrs-props

 ;; realized-face
 realized-face? realized-face-id realized-face-attrs
 realized-face-ansi-bytes

 ;; face cache
 face-cache? make-face-cache
 face-cache-by-id face-cache-next-id
 face-cache-lookup-or-realize!

 ;; named faces
 define-face! face-id-for-name

 ;; face merging
 merge-face-attrs face-id-with-overlay

 ;; predefined faces
 default-face region-face

 ;; global cache
 current-face-cache init-face-cache!)

;; ============================================================
;; Attribute keys
;; ============================================================

(define attr-foreground    'foreground)
(define attr-background    'background)
(define attr-weight        'weight)
(define attr-slant         'slant)
(define attr-underline     'underline)
(define attr-inverse-video 'inverse-video)

;; ============================================================
;; face-attrs — logical face properties
;; ============================================================

(struct face-attrs (props) #:transparent)

(define (make-face-attrs . kvs)
  (face-attrs
   (for/hash ([i (in-range 0 (length kvs) 2)])
     (values (list-ref kvs i) (list-ref kvs (add1 i))))))

(define (face-attrs-ref fa key [default #f])
  (hash-ref (face-attrs-props fa) key default))

;; ============================================================
;; Realized face — face-attrs + cached ANSI bytes
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
;; Face cache — attrs → face-id → ANSI bytes
;; ============================================================

(struct face-cache
  ([table #:mutable]   ; hash[attrs-hash → realized-face]
   [by-id #:mutable]   ; vector[face-id → realized-face]
   [next-id #:mutable]); nonnegative-integer
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
      (set-face-cache-by-id! fc
        (vector-append (face-cache-by-id fc) (vector rf)))
      rf)))

;; ============================================================
;; Named faces — symbol → face-attrs → face-id
;; ============================================================

(define named-face-table (make-hash))

(define (define-face! name attrs)
  (hash-set! named-face-table name attrs))

(define (face-id-for-name name [fc (current-face-cache)])
  (cond [(not fc) 0]
        [(hash-has-key? named-face-table name)
         (realized-face-id
          (face-cache-lookup-or-realize! fc
            (hash-ref named-face-table name)
            (color-depth)))]
        [else 0]))

(define (face-attrs-by-name name)
  (hash-ref named-face-table name (λ () (make-face-attrs))))

;; ============================================================
;; Face merging — overlay on top of base
;; ============================================================

(define (merge-face-attrs base overlay)
  (define base-props (face-attrs-props base))
  (define overlay-props (face-attrs-props overlay))
  (define merged (make-hash))
  (for ([(k v) (in-hash base-props)]) (hash-set! merged k v))
  (for ([(k v) (in-hash overlay-props)] #:when v) (hash-set! merged k v))
  (face-attrs merged))

(define (face-id-with-overlay base-face-name overlay-face-name
                               [fc (current-face-cache)])
  (and fc
       (let* ([base-attrs  (if base-face-name
                                (face-attrs-by-name base-face-name)
                                (face-attrs-by-name default-face))]
              [final-attrs (if overlay-face-name
                                (merge-face-attrs base-attrs
                                  (face-attrs-by-name overlay-face-name))
                                base-attrs)])
         (realized-face-id
          (face-cache-lookup-or-realize! fc final-attrs (color-depth))))))

;; ============================================================
;; Predefined faces
;; ============================================================

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

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)

  (parameterize ([color-depth 'truecolor])
    (init-face-cache!)
    (define id (face-id-for-name default-face))
    (check-equal? id 0)

    (define fc (current-face-cache))
    (define rf (vector-ref (face-cache-by-id fc) id))
    (check-true (realized-face? rf))
    (check-true (bytes? (realized-face-ansi-bytes rf))))

  (parameterize ([color-depth 'truecolor])
    (set-box! global-face-cache #f)
    (init-face-cache!)
    (define-face! 'keyword (make-face-attrs attr-foreground '(255 128 0)
                                            attr-weight 'bold))
    (define kid (face-id-for-name 'keyword))
    (check-true (> kid 0))
    (define fc (current-face-cache))
    (define rf (vector-ref (face-cache-by-id fc) kid))
    (check-true (> (bytes-length (realized-face-ansi-bytes rf)) 0)))

  (test-case "face merging"
    (parameterize ([color-depth 'truecolor])
      (set-box! global-face-cache #f)
      (init-face-cache!)
      (define base (make-face-attrs attr-foreground 1))
      (define overlay (make-face-attrs attr-background 8))
      (define merged (merge-face-attrs base overlay))
      (check-equal? (face-attrs-ref merged attr-foreground) 1)
      (check-equal? (face-attrs-ref merged attr-background) 8))))
