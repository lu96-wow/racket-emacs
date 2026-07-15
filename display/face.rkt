#lang racket

;; display/face.rkt — Face/color system using interval-map textprop
;;
;; Layers: face-attrs (logical) → realized-face (ANSI bytes) → face-cache

(require "../platform/ansi.rkt"
         "../kernel/textprop.rkt"
         "../kernel/font-lock.rkt")

(provide
 ;; face-attrs
 make-face-attrs face-attrs? face-attrs-ref
 attr-foreground attr-background attr-weight attr-slant
 attr-underline attr-inverse-video

 ;; realized-face
 realized-face? realized-face-id realized-face-attrs
 realized-face-ansi-bytes

 ;; face cache
 make-face-cache face-cache? face-cache-by-id face-cache-next-id
 face-cache-lookup-or-realize!

 ;; face registry
 defface face-defined? face-attrs-by-name
 default-face region-face mode-line-face isearch-face isearch-fail-face

 ;; rendering integration
 face-id-at-point face-id-with-overlay

 ;; face merging
 merge-face-attrs

 ;; global
 current-face-cache init-face-cache!

 ;; default colors
 current-default-foreground current-default-background)

;; ============================================================
;; face-attrs
;; ============================================================

(define attr-foreground 'foreground)
(define attr-background 'background)
(define attr-weight     'weight)
(define attr-slant      'slant)
(define attr-underline  'underline)
(define attr-inverse-video 'inverse-video)

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
  ([table #:mutable] [by-id #:mutable] [next-id #:mutable]) #:transparent)

(define (make-face-cache)
  (define default-rf (realize-face 0 (make-face-attrs) 'truecolor))
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
;; Face registry
;; ============================================================

(define face-registry (make-hash))
(define (defface name attrs) (hash-set! face-registry name attrs))
(define (face-defined? name) (hash-has-key? face-registry name))
(define (face-attrs-by-name name)
  (hash-ref face-registry name (λ () (make-face-attrs))))

(define default-face 'default)
(define region-face 'region)
(define mode-line-face 'mode-line)
(define isearch-face 'isearch)
(define isearch-fail-face 'isearch-fail)

(defface default-face (make-face-attrs))
(defface region-face (make-face-attrs attr-background 8))  ; dark gray bg, preserve fg
(defface mode-line-face (make-face-attrs attr-inverse-video #t))
(defface isearch-face (make-face-attrs attr-background 3 attr-foreground 0))
(defface isearch-fail-face (make-face-attrs attr-background 1 attr-foreground 7))

;; ============================================================
;; Face merging — overlay one face on another
;; ============================================================
;; When rendering region, isearch-match, etc., we want to overlay
;; a background color while preserving the text's foreground.

(define (merge-face-attrs base overlay)
  ;; Start with base attrs; overlay's non-#f values override.
  (define base-props (face-attrs-props base))
  (define overlay-props (face-attrs-props overlay))
  (define merged (make-hash))
  (for ([(k v) (in-hash base-props)]) (hash-set! merged k v))
  (for ([(k v) (in-hash overlay-props)] #:when v) (hash-set! merged k v))
  (face-attrs merged))

(define (face-id-with-overlay buf pos overlay-name)
  (define base-name (face-at-pos buf pos))
  (define fc (current-face-cache))
  ;; Pre-compute paren-depth background so we can merge it into the result.
  (define pd (get-paren-depth buf pos #f))
  (define pd-bg
    (and pd
         (let* ([pd-face-name (vector-ref paren-depth-faces pd)]
                [pd-attrs (face-attrs-by-name pd-face-name)])
           (face-attrs-ref pd-attrs attr-background #f))))
  ;; If overlay equals base face AND no paren-depth bg, just delegate.
  (if (and (eq? overlay-name base-name) (not pd-bg))
      (face-id-at-point buf pos)
      (let* ([base-attrs  (if base-name
                              (face-attrs-by-name base-name)
                              (effective-default-attrs))]
             [overlay-attrs (face-attrs-by-name overlay-name)]
             [merged-attrs (merge-face-attrs base-attrs overlay-attrs)]
             [final-attrs
              (if pd-bg
                  (merge-face-attrs merged-attrs (make-face-attrs attr-background pd-bg))
                  merged-attrs)]
             [rf (face-cache-lookup-or-realize! fc final-attrs (color-depth))])
        (realized-face-id rf))))

;; ============================================================
;; Rendering integration
;; ============================================================

(define (face-id-at-point buf pos)
  (define face-name (face-at-pos buf pos))
  (define fc (current-face-cache))
  (define base-attrs (if face-name
                         (face-attrs-by-name face-name)
                         (effective-default-attrs)))
  ;; If this position is inside brackets, merge the depth background.
  (define pd (get-paren-depth buf pos #f))
  (define merged-attrs
    (if pd
        (let* ([pd-face-name (vector-ref paren-depth-faces pd)]
               [pd-attrs (face-attrs-by-name pd-face-name)]
               [pd-bg (face-attrs-ref pd-attrs attr-background #f)])
          (if pd-bg
              (merge-face-attrs base-attrs (make-face-attrs attr-background pd-bg))
              base-attrs))
        base-attrs))
  (define rf (face-cache-lookup-or-realize! fc merged-attrs (color-depth)))
  (realized-face-id rf))

;; ============================================================
;; Default colors — dynamically configurable
;; ============================================================

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

;; ============================================================
;; Global face cache
;; ============================================================

(define global-face-cache (box #f))
(define (current-face-cache) (unbox global-face-cache))
(define (init-face-cache!)
  (unless (unbox global-face-cache)
    (set-box! global-face-cache (make-face-cache))))

;; ============================================================
;; Font-lock face definitions
;; ============================================================

(for-each (λ (pair) (unless (face-defined? (car pair)) (defface (car pair) (cdr pair))))
          (list (cons font-lock-string-face (make-face-attrs 'foreground 2))
                (cons font-lock-comment-face (make-face-attrs 'foreground 6 'weight 'light))
                (cons font-lock-keyword-face (make-face-attrs 'foreground 4 'weight 'bold))
                (cons font-lock-builtin-face (make-face-attrs 'foreground 4))
                (cons font-lock-constant-face (make-face-attrs 'foreground 1))
                (cons font-lock-function-name-face (make-face-attrs 'weight 'bold))
                (cons font-lock-type-face (make-face-attrs 'foreground 3 'weight 'bold))
                (cons font-lock-variable-name-face (make-face-attrs))
                ;; Rainbow parens — 12 depth levels, 30% opacity dark theme (2× brighter)
                ;; Circular rotated: orange (was 8) → 1, all others shifted
                (cons font-lock-paren-face-1  (make-face-attrs 'background (list 76 54 24)))
                (cons font-lock-paren-face-2  (make-face-attrs 'background (list 24 60 30)))
                (cons font-lock-paren-face-3  (make-face-attrs 'background (list 30 54 76)))
                (cons font-lock-paren-face-4  (make-face-attrs 'background (list 76 30 48)))
                (cons font-lock-paren-face-5  (make-face-attrs 'background (list 66 76 30)))
                (cons font-lock-paren-face-6  (make-face-attrs 'background (list 76 42 30)))
                (cons font-lock-paren-face-7  (make-face-attrs 'background (list 30 66 42)))
                (cons font-lock-paren-face-8  (make-face-attrs 'background (list 30 48 76)))
                (cons font-lock-paren-face-9  (make-face-attrs 'background (list 76 30 66)))
                (cons font-lock-paren-face-10 (make-face-attrs 'background (list 76 64 30)))
                (cons font-lock-paren-face-11 (make-face-attrs 'background (list 30 76 70)))
                (cons font-lock-paren-face-12 (make-face-attrs 'background (list 60 30 76)))))
