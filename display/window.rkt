#lang racket

;; display/window.rkt — Window tree + layout
;;
;; Three layers, cleanly separated:
;;   1. Data — tree types (leaf, split) + frame container
;;   2. Pure calc — layout-calc, focus-list, window-at
;;   3. Apply — layout-frame! (calc + write geometry), tree ops
;;
;; Geometry lives in frame, NOT in nodes.  Layout is a pure function
;; tree × w × h → (listof (cons node rect)).  Users replace functions,
;; not register hooks.

(require "../kernel/buffer.rkt"
         "../kernel/text.rkt"
         "../kernel/gap/marker.rkt")

(provide
 ;; ── types ──
 leaf? leaf leaf-buffer leaf-start leaf-pointm leaf-hscroll
 split? split split-direction split-children
 frame? frame-tree frame-geometry frame-selected frame-width frame-height
 set-leaf-buffer! set-leaf-start! set-leaf-pointm! set-leaf-hscroll!
 set-split-children!
 set-frame-geometry! set-frame-selected! set-frame-width! set-frame-height!
 set-frame-tree!

 ;; ── constructors ──
 make-leaf
 init-frame

 ;; ── pure calc ──
 layout-calc focus-list window-at
 rect-top rect-left rect-rows rect-cols
 next-leaf prev-leaf

 ;; ── apply ──
 layout-frame!
 split-leaf! delete-leaf! set-leaf-buffer-in-tree!

 ;; ── query ──
 leaf-geometry leaf-selected?
 frame-leaf-list selected-leaf current-frame

 ;; ── point ──
 leaf-point set-leaf-point!)

;; ============================================================
;; Types
;; ============================================================

;; Leaf — a buffer window.  Geometry is extrinsic (in frame-geometry).
(struct leaf
  ([buffer #:mutable]   ; buffer?
   [start #:mutable]    ; marker? — scroll anchor
   [pointm #:mutable]   ; marker? — cursor
   [hscroll #:mutable]) ; integer
  #:transparent)

;; Split — divides space among children.
;; direction: 'horizontal (left→right) or 'vertical (top→bottom)
(struct split
  (direction            ; 'horizontal | 'vertical
   [children #:mutable]); (listof (or/c leaf? split?))
  #:transparent)

;; rect — computed geometry: (vector top left rows cols)
(define (rect-top r)    (vector-ref r 0))
(define (rect-left r)   (vector-ref r 1))
(define (rect-rows r)   (vector-ref r 2))
(define (rect-cols r)   (vector-ref r 3))
(define (make-rect t l r c) (vector t l r c))

;; Frame — ties tree + geometry + focus + dimensions
(struct frame
  ([tree #:mutable]      ; (or/c leaf? split?)
   [geometry #:mutable]  ; (hash leaf? rect)
   [selected #:mutable]  ; leaf?
   [width #:mutable]     ; integer
   [height #:mutable])   ; integer
  #:transparent)

;; ============================================================
;; Constructors
;; ============================================================

(define (make-leaf buf)
  (define tx (buffer-text buf))
  (leaf buf
        (text-marker! tx 0 #f)       ; start
        (buffer-point-marker buf)    ; pointm
        0))                           ; hscroll

(define (init-frame buf width height)
  (define root (make-leaf buf))
  (define frm (frame root (make-hash) root width height))
  (layout-frame! frm)
  (current-frame frm)
  frm)

;; ============================================================
;; Global frame parameter
;; ============================================================

(define current-frame (make-parameter #f))
(define (selected-leaf) (define frm (current-frame)) (and frm (frame-selected frm)))

;; ============================================================
;; Pure: focus-list — DFS left-to-right leaf order
;; ============================================================

(define (focus-list tree)
  (let dfs ([t tree])
    (match t
      [(? leaf?) (list t)]
      [(split _ children) (append-map dfs children)])))

;; ============================================================
;; Pure: next-leaf / prev-leaf — cyclic
;; ============================================================

(define (next-leaf frm)
  (define leaves (focus-list (frame-tree frm)))
  (define sel (frame-selected frm))
  (define idx (or (index-of leaves sel) 0))
  (list-ref leaves (modulo (add1 idx) (length leaves))))

(define (prev-leaf frm)
  (define leaves (focus-list (frame-tree frm)))
  (define sel (frame-selected frm))
  (define idx (or (index-of leaves sel) 0))
  (list-ref leaves (modulo (sub1 idx) (length leaves))))

(define (index-of lst item)
  (let loop ([xs lst] [i 0])
    (cond [(null? xs) #f]
          [(eq? (car xs) item) i]
          [else (loop (cdr xs) (add1 i))])))

;; ============================================================
;; Pure: layout-calc
;;   tree × width × height → (listof (cons leaf? rect))
;; ============================================================

(define (layout-calc tree width height)
  (let loop ([t tree] [top 0] [left 0] [rows height] [cols width])
    (match t
      [(? leaf?)
       (list (cons t (make-rect top left rows cols)))]
      [(split dir children)
       (define n (length children))
       (if (zero? n)
           '()
           (if (eq? dir 'horizontal)
               (let sub ([cs children] [l left] [acc '()])
                 (if (null? cs)
                     (reverse acc)
                     (let* ([c (car cs)]
                            [c-cols (max 1 (quotient (- cols (- l left)) (length cs)))]
                            [result (loop c top l rows c-cols)])
                       (sub (cdr cs) (+ l c-cols) (append result acc)))))
               (let sub ([cs children] [t top] [acc '()])
                 (if (null? cs)
                     (reverse acc)
                     (let* ([c (car cs)]
                            [c-rows (max 1 (quotient (- rows (- t top)) (length cs)))]
                            [result (loop c t left c-rows cols)])
                       (sub (cdr cs) (+ t c-rows) (append result acc)))))))])))

;; ============================================================
;; Pure: window-at — which leaf contains (y, x)?
;; ============================================================

(define (window-at geo y x)
  (for/or ([(lf rect) (in-hash geo)])
    (and (<= (rect-top rect) y (+ (rect-top rect) (rect-rows rect) -1))
         (<= (rect-left rect) x (+ (rect-left rect) (rect-cols rect) -1))
         lf)))

;; ============================================================
;; Apply: layout-frame! — calc + write geometry
;; ============================================================

(define (layout-frame! frm)
  (define rects (layout-calc (frame-tree frm) (frame-width frm) (frame-height frm)))
  (define geo (make-hash))
  (for ([p (in-list rects)])
    (hash-set! geo (car p) (cdr p)))
  (set-frame-geometry! frm geo)
  ;; Ensure selected leaf is still in tree
  (define leaves (focus-list (frame-tree frm)))
  (unless (and (frame-selected frm) (memq (frame-selected frm) leaves))
    (when (pair? leaves)
      (set-frame-selected! frm (car leaves))))
  frm)

;; ============================================================
;; Query helpers
;; ============================================================

(define (leaf-geometry frm lf)
  (hash-ref (frame-geometry frm) lf (λ () #f)))

(define (leaf-selected? frm lf)
  (eq? lf (frame-selected frm)))

(define (frame-leaf-list frm)
  (focus-list (frame-tree frm)))

;; ============================================================
;; Point helpers
;; ============================================================

(define (leaf-point lf)
  (define buf (leaf-buffer lf))
  (define tx (and buf (buffer-text buf)))
  (define pm (leaf-pointm lf))
  (if (and tx pm) (text-marker-pos tx pm) 0))

(define (set-leaf-point! lf pos)
  (define tx (and (leaf-buffer lf) (buffer-text (leaf-buffer lf))))
  (define pm (leaf-pointm lf))
  (when (and tx pm) (text-set-marker-pos! tx pm pos)))

;; ============================================================
;; Tree operations (pure → then apply)
;; ============================================================

;; replace-in-tree : tree old-node new-node → tree
(define (replace-in-tree tree target replacement)
  (cond [(eq? tree target) replacement]
        [(split? tree)
         (split (split-direction tree)
                (map (λ (c) (replace-in-tree c target replacement))
                     (split-children tree)))]
        [else tree]))

;; remove-from-tree : tree target → tree
(define (remove-from-tree tree target)
  (match tree
    [(? leaf?) (if (eq? tree target) 'removed tree)]
    [(split dir children)
     (define new-children
       (filter (λ (c) (not (eq? c 'removed)))
               (map (λ (c) (remove-from-tree c target)) children)))
     (match new-children
       ['() 'removed]
       [(list only) only]  ; collapse single-child split
       [_ (split dir new-children)])]))

;; split-leaf! : frame leaf buffer direction → leaf
;; direction: 'vertical (new below) or 'horizontal (new right)
(define (split-leaf! frm lf buf direction)
  (define new (make-leaf buf))
  (define inner (split direction (list lf new)))
  (set-frame-tree! frm (replace-in-tree (frame-tree frm) lf inner))
  (layout-frame! frm)
  new)

;; delete-leaf! : frame leaf → void
(define (delete-leaf! frm lf)
  (define new-tree (remove-from-tree (frame-tree frm) lf))
  (when (leaf? new-tree)
    (set! new-tree (split 'vertical (list new-tree)))) ; never rootless
  (set-frame-tree! frm new-tree)
  (layout-frame! frm))

;; set-leaf-buffer-in-tree! : frame leaf buffer → void
(define (set-leaf-buffer-in-tree! frm lf buf)
  (set-leaf-buffer! lf buf)
  (define tx (buffer-text buf))
  (set-leaf-start! lf (text-marker! tx 0 #f))
  (set-leaf-pointm! lf (buffer-point-marker buf))
  (set-leaf-hscroll! lf 0))
