#lang racket

;; display/face.rkt — Face system: face-attrs → face-id → ANSI
;;
;; ============================================================================
;; Three layers:
;;   1. face-attrs  — logical properties (color, weight, slant)
;;   2. face-cache  — maps attrs → integer face-id, caches ANSI bytes
;;   3. named faces — symbol → face-attrs (keyword, comment, string, ...)
;;
;; ============================================================================
;; Computation vs Application
;; ============================================================================
;;
;;   ── Pure Queries ──
;;     face-attrs-ref     face-attrs key → value
;;
;;   ── Imperative (register faces into cache) ──
;;     define-face!                 name attrs → void
;;     face-id-for-name             name [fc] → face-id
;;     face-id-with-overlay-id      base-fid overlay-fid fc → merged-fid
;;     face-cache-lookup-or-realize! fc attrs depth → realized-face
;;
;; ============================================================================
;; Face-Id Convention
;; ============================================================================
;;
;;   face-id 0 = default (no highlighting)
;;   face-id 1..N = registered faces (assigned by face-cache in order)
;;
;;   Language setup calls define-face! then face-id-for-name for each face.
;;   The returned face-id is what the colorer writes to the gap buffer.
;;   The render reads face-ids directly from the gap (via kernel/data/face.rkt).
;;   The terminal flush uses face-cache-by-id to get ANSI bytes.
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

 ;; ── face cache ──
 face-cache? make-face-cache
 face-cache-by-id face-cache-next-id
 face-cache-lookup-or-realize!

 ;; ── named faces ──
 define-face! face-id-for-name

 ;; ── face merging ──
 merge-face-attrs face-id-with-overlay-id

 ;; ── predefined faces ──
 default-face region-face

 ;; ── global cache ──
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
  ;; Read a property from face-attrs.
  (unless (face-attrs? fa)
    (raise-argument-error 'face-attrs-ref "face-attrs?" fa))
  (hash-ref (face-attrs-props fa) key default))

;; ============================================================
;; Realized face — face-attrs + cached ANSI bytes
;; ============================================================

(struct realized-face (id attrs ansi-bytes) #:transparent)

(define (realize-face id attrs depth)
  ;; Produce ANSI escape bytes for a set of face attributes.
  ;; Contract: attrs must be a face-attrs?, depth one of 'truecolor|'256|'16|'none.
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
;; Face cache — attrs → face-id → ANSI bytes
;; ============================================================

(struct face-cache
  ([table #:mutable]    ; hash[attrs-hash → realized-face]
   [by-id #:mutable]    ; vector[face-id → realized-face]
   [next-id #:mutable]) ; next available face-id
  #:transparent)

(define (make-face-cache)
  ;; Default face (id=0) has no attributes — transparent.
  (define default-rf (realize-face 0 (make-face-attrs) (color-depth)))
  (face-cache (make-hash) (vector default-rf) 1))

(define (face-cache-lookup-or-realize! fc attrs depth)
  ;; Look up face-attrs in the cache.  If not present, assign a new
  ;; face-id, realize ANSI bytes, and store.  Returns realized-face.
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
;; Named faces — symbol → face-attrs → face-id
;; ============================================================

(define named-face-table (make-hash))

(define (define-face! name attrs)
  ;; Register a named face.  Does NOT assign a face-id yet —
  ;; face-id-for-name does that on first lookup.
  (unless (symbol? name)
    (raise-argument-error 'define-face! "symbol?" name))
  (unless (face-attrs? attrs)
    (raise-argument-error 'define-face! "face-attrs?" attrs))
  (hash-set! named-face-table name attrs))

(define (face-id-for-name name [fc (current-face-cache)])
  ;; Resolve a named face to its face-id.
  ;; Realizes the face through the cache, assigning a stable face-id.
  ;; Returns face-id or 0 if face is not registered.
  (cond [(not fc) 0]
        [(hash-has-key? named-face-table name)
         (realized-face-id
          (face-cache-lookup-or-realize! fc
            (hash-ref named-face-table name)
            (color-depth)))]
        [else
         (unless (memq name '(default region))
           (log-warning "face-id-for-name: face not registered: ~a" name))
         0]))

(define (face-attrs-by-name name)
  ;; Get face-attrs for a named face.  Returns default attrs if not found.
  (hash-ref named-face-table name (λ () (make-face-attrs))))

;; ============================================================
;; Face merging
;; ============================================================

(define (merge-face-attrs base overlay)
  ;; Merge two face-attrs: overlay properties override base properties.
  ;; Returns a new face-attrs.
  (define base-props (face-attrs-props base))
  (define overlay-props (face-attrs-props overlay))
  (define merged (make-hash))
  (for ([(k v) (in-hash base-props)]) (hash-set! merged k v))
  (for ([(k v) (in-hash overlay-props)] #:when v) (hash-set! merged k v))
  (face-attrs merged))

(define (face-id-with-overlay-id base-fid overlay-fid fc)
  ;; Given two face-ids, produce a merged face-id through the cache.
  ;; Looks up attrs for both IDs, merges them, and realizes the result.
  ;; Contracts:
  ;;   - base-fid, overlay-fid: valid face-ids in face-cache
  ;;   - fc: face-cache?
  ;; Returns merged face-id, or base-fid if overlay is 0 or cache is invalid.
  (cond [(not fc) base-fid]
        [(zero? overlay-fid) base-fid]
        [(zero? base-fid) overlay-fid]
        [else
         (define by-id (face-cache-by-id fc))
         (cond
           [(or (>= base-fid (vector-length by-id))
                (>= overlay-fid (vector-length by-id)))
            ;; Invalid face-ids — return base as-is
            base-fid]
           [else
            (define base-attrs (realized-face-attrs (vector-ref by-id base-fid)))
            (define overlay-attrs (realized-face-attrs (vector-ref by-id overlay-fid)))
            (define merged-attrs (merge-face-attrs base-attrs overlay-attrs))
            (realized-face-id
             (face-cache-lookup-or-realize! fc merged-attrs (color-depth)))])]))

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

(define (current-face-cache)
  (unbox global-face-cache))

(define (init-face-cache!)
  (unless (unbox global-face-cache)
    (set-box! global-face-cache (make-face-cache))))
