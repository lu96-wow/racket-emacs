#lang racket

;; display/window.rkt — Window & frame primitives
;;
;; Window tree + frame = the view layer.  Each leaf window displays
;; a buffer.  Internal windows split space into children.
;; Adapted from protocol/window.rkt to use rebuild kernel/buffer API.

(require "../kernel/buffer.rkt"
         "../kernel/text.rkt"
         "../kernel/gap/marker.rkt")

(provide
 ;; window
 window? window-parent window-children window-horizontal?
 window-top window-left window-rows window-cols
 window-hscroll set-window-hscroll!
 window-buffer set-window-buffer! window-start window-pointm window-selected?
 set-window-parent! set-window-children! set-window-horizontal?!
 set-window-top! set-window-left! set-window-rows! set-window-cols!
 set-window-start! set-window-pointm!
 set-window-selected?!
 window-desired-ratio set-window-desired-ratio!
 window-params set-window-params!

 ;; frame
 frame? frame-root-window frame-selected-window
 frame-width frame-height
 set-frame-root-window! set-frame-selected-window!
 set-frame-width! set-frame-height!

 ;; query
 window-leaf? window-live-p
 frame-window-list
 selected-window current-frame

 ;; create
 make-leaf-window make-internal-window
 init-root-frame

 ;; layout
 layout-frame! split-window! delete-window!
 window-desired-rows set-window-desired-rows!

 ;; utility
 other-visible-window balance-windows!

 ;; point
 window-point set-window-point!)

;; ============================================================
;; Window
;; ============================================================

(struct window
  ([parent #:mutable] [children #:mutable] [horizontal? #:mutable]
   [top #:mutable] [left #:mutable] [rows #:mutable] [cols #:mutable]
   [buffer #:mutable] [start #:mutable] [pointm #:mutable] [hscroll #:mutable]
   [selected? #:mutable]
   [desired-ratio #:mutable]  ; #f = equal share, number = fraction of parent
   [params #:mutable])         ; hasheq — user key-value store
  #:transparent)

;; ============================================================
;; Frame
;; ============================================================

(struct frame
  ([root-window #:mutable]
   [selected-window #:mutable] [width #:mutable] [height #:mutable])
  #:transparent)

;; ============================================================
;; Queries
;; ============================================================

(define (window-leaf? w) (and (null? (window-children w)) (buffer? (window-buffer w))))
(define (window-live-p w) (window-leaf? w))

(define (frame-window-list frm)
  (define acc '())
  (let dfs ([w (frame-root-window frm)])
    (when w
      (if (window-leaf? w)
          (set! acc (cons w acc))
          (for ([child (in-list (window-children w))]) (dfs child)))))
  (reverse acc))

(define current-frame (make-parameter #f))
(define (selected-window) (define frm (current-frame)) (and frm (frame-selected-window frm)))

;; ============================================================
;; Constructors
;; ============================================================

(define (make-leaf-window buf)
  (define tx (buffer-text buf))
  (define start-m (text-marker! tx 0 #f))
  ;; Use buffer's point marker; set initial position to buffer point
  (define pt-m (buffer-point-marker buf))
  (window #f '() #f 0 0 0 0 buf start-m pt-m 0 #f #f (make-hasheq)))

(define (make-internal-window horizontal?)
  (window #f '() horizontal? 0 0 0 0 #f #f #f 0 #f #f (make-hasheq)))
;; ============================================================
;; ============================================================
;; Window tree operations
;; ============================================================

;; split-window! — split an existing leaf window, returning the new window
;; horizontal? = #f → vertical split (new window below)
;; horizontal? = #t → horizontal split (new window to the right)
(define (split-window! w buf horizontal?)
  (unless (window-leaf? w)
    (error 'split-window! "can only split a leaf window"))
  (define new-win (make-leaf-window buf))
  (define internal (make-internal-window horizontal?))
  (set-window-children! internal (list w new-win))
  (define parent (window-parent w))
  (define siblings (and parent (window-children parent)))
  (if parent
      (let ([idx (index-of siblings w)])
        (set-window-children! parent (list-update siblings idx internal))
        (set-window-parent! internal parent)
        (set-window-parent! w internal)
        (set-window-parent! new-win internal))
      ;; w is the root — replace frame root
      (begin
        (set-frame-root-window! (current-frame) internal)
        (set-window-parent! w internal)
        (set-window-parent! new-win internal)))
  (layout-frame! (current-frame))
  new-win)

;; delete-window! — delete a window, giving its space to siblings
(define (delete-window! w [frm (current-frame)])
  (define parent (window-parent w))
  (unless parent (error 'delete-window! "cannot delete sole window"))
  (define siblings (window-children parent))
  (define remaining (remq w siblings))
  (cond [(null? remaining)
         (error 'delete-window! "internal error: no siblings")]
        [(null? (cdr remaining))
         ;; Only one sibling left — collapse the internal node
         (define only-sib (car remaining))
         (define grandparent (window-parent parent))
         (if grandparent
             (let* ([gp-children (window-children grandparent)]
                    [p-idx (index-of gp-children parent)])
               (set-window-children! grandparent
                 (list-update gp-children p-idx only-sib))
               (set-window-parent! only-sib grandparent))
             ;; parent was root — promote only-sib to root
             (begin
               (set-frame-root-window! frm only-sib)
               (set-window-parent! only-sib #f)))]
        [else
         ;; More than one sibling — just remove w
         (set-window-children! parent remaining)])
  (layout-frame! frm))

;; other-visible-window — return any visible window other than the selected one
(define (other-visible-window [frm (current-frame)])
  (define sel (selected-window))
  (for/or ([w (in-list (frame-window-list frm))])
    (and (not (eq? w sel)) w)))

;; balance-windows! — reset all desired-ratio to #f (equal splits)
(define (balance-windows! [frm (current-frame)])
  (let dfs ([w (frame-root-window frm)])
    (when w
      (cond [(null? (window-children w))
             (set-window-desired-ratio! w #f)]
            [else
             (set-window-desired-ratio! w #f)
             (for ([child (in-list (window-children w))])
               (set-window-desired-ratio! child #f)
               (dfs child))])))
  (layout-frame! frm))

;; ============================================================
;; Helpers
;; ============================================================

(define (index-of lst item)
  (let loop ([xs lst] [i 0])
    (cond [(null? xs) #f]
          [(eq? (car xs) item) i]
          [else (loop (cdr xs) (add1 i))])))

(define (list-update lst idx new)
  (append (take lst idx) (list new) (drop lst (add1 idx))))

;; ============================================================
;; Point — get/set point for a window via its buffer's marker
;; ============================================================

(define (window-point w)
  (define tx (and (window-buffer w) (buffer-text (window-buffer w))))
  (define pm (window-pointm w))
  (if (and tx pm) (text-marker-pos tx pm) 0))

(define (set-window-point! w pos)
  (define tx (and (window-buffer w) (buffer-text (window-buffer w))))
  (define pm (window-pointm w))
  (when (and tx pm) (text-set-marker-pos! tx pm pos)))

;; ============================================================
;; Layout
;; ============================================================

(define window-desired-rows-table (make-hasheq))
(define (window-desired-rows w) (hash-ref window-desired-rows-table w (λ () 1)))
(define (set-window-desired-rows! w rows) (hash-set! window-desired-rows-table w rows))

(define (init-root-frame buf width height)
  (define root (make-leaf-window buf))
  (set-window-selected?! root #t)
  (define frm (frame root root width height))
  (layout-frame! frm)
  (current-frame frm)
  frm)

(define (layout-frame! frm)
  (define fw (frame-width frm)) (define fh (frame-height frm))
  (layout-subtree! (frame-root-window frm) 0 0 fh fw))

(define (layout-subtree! w top left rows cols)
  (set-window-top! w top) (set-window-left! w left)
  (set-window-rows! w rows) (set-window-cols! w cols)
  (define children (window-children w))
  (unless (null? children)
    (define ratios (compute-ratios children))
    (if (window-horizontal? w)
        (let loop ([cs children] [rs ratios] [l left])
          (unless (null? cs)
            (let* ([child-cols (max 1 (exact-round (* cols (car rs))))]
                   [remaining-cols (- cols child-cols)]
                   [rest-ratio-sum (- 1.0 (car rs))])
              ;; Re-normalize remaining ratios for remaining children
              (define norm-rest (if (> (length (cdr cs)) 0)
                                   (map (λ (r) (if (> rest-ratio-sum 0)
                                                    (/ r rest-ratio-sum)
                                                    (/ 1.0 (length (cdr cs)))))
                                        (cdr rs))
                                   '()))
              (layout-subtree! (car cs) top l rows child-cols)
              (loop (cdr cs) norm-rest (+ l child-cols)))))
        (let loop ([cs children] [rs ratios] [t top])
          (unless (null? cs)
            (let* ([child-rows (max 1 (exact-round (* rows (car rs))))]
                   [remaining-rows (- rows child-rows)]
                   [rest-ratio-sum (- 1.0 (car rs))])
              (define norm-rest (if (> (length (cdr cs)) 0)
                                   (map (λ (r) (if (> rest-ratio-sum 0)
                                                    (/ r rest-ratio-sum)
                                                    (/ 1.0 (length (cdr cs)))))
                                        (cdr rs))
                                   '()))
              (layout-subtree! (car cs) t left child-rows cols)
              (loop (cdr cs) norm-rest (+ t child-rows))))))))

;; compute-ratios — resolve desired-ratio → normalized fractions
(define (compute-ratios children)
  (define explicit
    (for/list ([c (in-list children)])
      (window-desired-ratio c)))
  (define sum-explicit (for/sum ([r (in-list explicit)] #:when r) r))
  (cond
    ;; All explicit, sum to 1.0 — use as-is
    [(and (> sum-explicit 0) (andmap values explicit) (<= (abs (- sum-explicit 1.0)) 0.001))
     explicit]
    ;; Partial explicit — remaining children share rest equally
    [(> sum-explicit 0)
     (define unspecified-count (count (λ (r) (not r)) explicit))
     (define rest (/ (- 1.0 sum-explicit) unspecified-count))
     (for/list ([r (in-list explicit)])
       (or r rest))]
    ;; No explicit ratios — equal split
    [else
     (define n (length children))
     (for/list ([c (in-list children)]) (/ 1.0 n))]))
