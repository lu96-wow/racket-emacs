#lang racket

;; display/face.rkt — Face system: face-attrs → face-id → ANSI
;;
;; ============================================================================
;; Three layers:
;;   1. face-attrs    — logical properties (color, weight, slant)
;;   2. face-cache    — maps attrs → integer face-id, caches ANSI bytes
;;   3. face-registry — binds named faces to attrs + owns the cache
;;
;; ============================================================================
;; Architecture — explicit state, no globals
;; ============================================================================
;;
;;   face-registry  =  named-table (symbol → face-attrs)
;;                   + face-cache   (attrs → face-id → ANSI bytes)
;;
;;   The caller creates a face-registry, registers faces into it,
;;   then passes it through the pipeline.  No hidden global state.
;;
;; ============================================================================
;; Computation vs Application
;; ============================================================================
;;
;;   ── Pure ──
;;     face-attrs-ref              fa key → value
;;     merge-face-attrs            base overlay → merged-attrs
;;
;;   ── Imperative (mutate registry or cache) ──
;;     define-face!                 reg name attrs → void
;;     face-id-for-name             reg name → face-id
;;     face-id-with-overlay-id      base-fid overlay-fid fc → merged-fid
;;     face-cache-lookup-or-realize! fc attrs depth → realized-face
;;
;; ============================================================================
;; Dependencies: platform/ansi.rkt (format strings only, no state)
;; ============================================================================

(require "../platform/ansi.rkt")

(provide
 ;; ── attribute keys ──
 attr-foreground attr-background attr-weight attr-slant
 attr-underline attr-inverse-video

 ;; ── face-attrs ──
 face-attrs? make-face-attrs face-attrs-ref face-attrs-props

 ;; ── realized-face ──
 realized-face? realized-face-id realized-face-attrs
 realized-face-ansi-bytes

 ;; ── face cache (low-level: attrs → face-id → ANSI) ──
 face-cache? make-face-cache
 face-cache-by-id face-cache-next-id
 face-cache-lookup-or-realize!

 ;; ── face registry (high-level: named faces + cache) ──
 face-registry? make-face-registry
 face-registry-cache face-registry-named
 define-face! face-id-for-name face-attrs-by-name

 ;; ── face merging ──
 merge-face-attrs face-id-with-overlay-id

 ;; ── predefined face name constants ──
 default-face region-face)

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
;; face-attrs — logical face properties (pure data)
;; ============================================================

(struct face-attrs (props) #:transparent)

(define (make-face-attrs . kvs)
  (face-attrs
   (for/hash ([i (in-range 0 (length kvs) 2)])
     (values (list-ref kvs i) (list-ref kvs (add1 i))))))

(define (face-attrs-ref fa key [default #f])
  (unless (face-attrs? fa)
    (raise-argument-error 'face-attrs-ref "face-attrs?" fa))
  (hash-ref (face-attrs-props fa) key default))

;; ============================================================
;; Realized face — face-attrs + cached ANSI bytes
;; ============================================================

(struct realized-face (id attrs ansi-bytes) #:transparent)

(define (realize-face id attrs depth)
  (unless (face-attrs? attrs)
    (raise-argument-error 'realize-face "face-attrs?" attrs))
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
;; Face cache — attrs → face-id → ANSI bytes (low-level)
;; ============================================================

(struct face-cache
  ([table #:mutable]    ; hash[attrs-hash → realized-face]
   [by-id #:mutable]    ; vector[face-id → realized-face]
   [next-id #:mutable]) ; next available face-id
  #:transparent)

(define (make-face-cache)
  (define default-rf (realize-face 0 (make-face-attrs) (color-depth)))
  (face-cache (make-hash) (vector default-rf) 1))

(define (face-cache-lookup-or-realize! fc attrs depth)
  (unless (face-cache? fc)
    (raise-argument-error 'face-cache-lookup-or-realize! "face-cache?" fc))
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
;; Face registry — named faces + cache (high-level, explicit ownership)
;; ============================================================

(struct face-registry
  (named   ; hash[symbol → face-attrs] — mutable
   cache)  ; face-cache? — attrs → face-id → ANSI
  #:transparent)

(define (make-face-registry)
  (face-registry (make-hash) (make-face-cache)))

;; ============================================================
;; Named face operations — all take registry explicitly
;; ============================================================

(define (define-face! reg name attrs)
  (unless (face-registry? reg)
    (raise-argument-error 'define-face! "face-registry?" reg))
  (unless (symbol? name)
    (raise-argument-error 'define-face! "symbol?" name))
  (unless (face-attrs? attrs)
    (raise-argument-error 'define-face! "face-attrs?" attrs))
  (hash-set! (face-registry-named reg) name attrs))

(define (face-id-for-name reg name)
  (unless (face-registry? reg)
    (raise-argument-error 'face-id-for-name "face-registry?" reg))
  (unless (symbol? name)
    (raise-argument-error 'face-id-for-name "symbol?" name))
  (define named (face-registry-named reg))
  (define fc    (face-registry-cache reg))
  (cond [(hash-has-key? named name)
         (realized-face-id
          (face-cache-lookup-or-realize! fc
            (hash-ref named name)
            (color-depth)))]
        [else
         (log-warning "face-id-for-name: face not registered: ~a" name)
         0]))

(define (face-attrs-by-name reg name)
  (unless (face-registry? reg)
    (raise-argument-error 'face-attrs-by-name "face-registry?" reg))
  (hash-ref (face-registry-named reg) name (λ () (make-face-attrs))))

;; ============================================================
;; Face merging (uses face-cache, not registry)
;; ============================================================

(define (merge-face-attrs base overlay)
  (define base-props (face-attrs-props base))
  (define overlay-props (face-attrs-props overlay))
  (define merged (make-hash))
  (for ([(k v) (in-hash base-props)]) (hash-set! merged k v))
  (for ([(k v) (in-hash overlay-props)] #:when v) (hash-set! merged k v))
  (face-attrs merged))

(define (face-id-with-overlay-id base-fid overlay-fid fc)
  (cond [(not fc) base-fid]
        [(zero? overlay-fid) base-fid]
        [(zero? base-fid) overlay-fid]
        [else
         (define by-id (face-cache-by-id fc))
         (cond
           [(or (>= base-fid (vector-length by-id))
                (>= overlay-fid (vector-length by-id)))
            base-fid]
           [else
            (define base-attrs (realized-face-attrs (vector-ref by-id base-fid)))
            (define overlay-attrs (realized-face-attrs (vector-ref by-id overlay-fid)))
            (define merged-attrs (merge-face-attrs base-attrs overlay-attrs))
            (realized-face-id
             (face-cache-lookup-or-realize! fc merged-attrs (color-depth)))])]))

;; ============================================================
;; Predefined face name constants (registration left to caller)
;; ============================================================

(define default-face 'default)
(define region-face 'region)
