#lang racket

;; display/window.rkt — Window (pure viewport into a buffer)
;;
;; A window is a viewport: it maps a buffer region onto screen space.
;; It owns NO rendering logic, NO event handling, NO keymap.
;; All it knows: which buffer, where the scroll is, where the cursor is.
;;
;; Three layers:
;;   1. Data — leaf/split/frame with geometry
;;   2. Pure calc — layout-calc (tree × w × h → rects)
;;   3. Apply — layout-frame! (calc + write geometry)

(require "../kernel/buffer.rkt"
         "../kernel/text.rkt"
         "../kernel/gap/marker.rkt")

(provide
 ;; ── viewport types ──
 leaf? leaf leaf-buffer leaf-start leaf-pointm leaf-hscroll
 split? split split-direction split-children
 frame? frame-tree frame-geometry frame-selected frame-width frame-height

 ;; ── mutation ──
 set-leaf-buffer! set-leaf-start! set-leaf-pointm! set-leaf-hscroll!
 set-split-children!
 set-frame-geometry! set-frame-selected! set-frame-width! set-frame-height!
 set-frame-tree!

 ;; ── constructors ──
 make-leaf init-frame

 ;; ── pure calc ──
 layout-calc focus-list
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

(struct leaf
  ([buffer #:mutable]
   [start #:mutable]    ; marker? — scroll anchor
   [pointm #:mutable]   ; marker? — cursor
   [hscroll #:mutable]) ; integer
  #:transparent)

(struct split
  (direction            ; 'horizontal | 'vertical
   [children #:mutable]); (listof (or/c leaf? split?))
  #:transparent)

(define (rect-top r)    (vector-ref r 0))
(define (rect-left r)   (vector-ref r 1))
(define (rect-rows r)   (vector-ref r 2))
(define (rect-cols r)   (vector-ref r 3))
(define (make-rect t l r c) (vector t l r c))

(struct frame
  ([tree #:mutable]
   [geometry #:mutable]  ; (hash leaf? rect)
   [selected #:mutable]  ; leaf?
   [width #:mutable]
   [height #:mutable])
  #:transparent)

;; ============================================================
;; Constructors
;; ============================================================

(define (make-leaf buf)
  (define tx (buffer-text buf))
  (leaf buf
        (text-marker! tx 0 #f)
        (buffer-point-marker buf)
        0))

(define (init-frame buf width height)
  (define root (make-leaf buf))
  (define frm (frame root (make-hash) root width height))
  (layout-frame! frm)
  (current-frame frm)
  frm)

;; ============================================================
;; Global frame
;; ============================================================

(define current-frame (make-parameter #f))
(define (selected-leaf) (define frm (current-frame)) (and frm (frame-selected frm)))

;; ============================================================
;; Pure: focus-list — DFS left-to-right
;; ============================================================

(define (focus-list tree)
  (let dfs ([t tree])
    (match t
      [(? leaf?) (list t)]
      [(split _ children) (append-map dfs children)])))

;; ============================================================
;; Pure: next-leaf / prev-leaf
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
;; ============================================================

(define (layout-calc tree width height)
  (let loop ([t tree] [top 0] [left 0] [rows height] [cols width])
    (match t
      [(? leaf?)
       (list (cons t (make-rect top left rows cols)))]
      [(split dir children)
       (define n (length children))
       (if (zero? n) '()
           (if (eq? dir 'horizontal)
               (let sub ([cs children] [l left] [acc '()])
                 (if (null? cs) (reverse acc)
                     (let* ([c (car cs)]
                            [c-cols (max 1 (quotient (- cols (- l left)) (length cs)))]
                            [result (loop c top l rows c-cols)])
                       (sub (cdr cs) (+ l c-cols) (append result acc)))))
               (let sub ([cs children] [t top] [acc '()])
                 (if (null? cs) (reverse acc)
                     (let* ([c (car cs)]
                            [c-rows (max 1 (quotient (- rows (- t top)) (length cs)))]
                            [result (loop c t left c-rows cols)])
                       (sub (cdr cs) (+ t c-rows) (append result acc)))))))])))

;; ============================================================
;; Apply: layout-frame!
;; ============================================================

(define (layout-frame! frm)
  (define rects (layout-calc (frame-tree frm) (frame-width frm) (frame-height frm)))
  (define geo (make-hash))
  (for ([p (in-list rects)])
    (hash-set! geo (car p) (cdr p)))
  (set-frame-geometry! frm geo)
  (define leaves (focus-list (frame-tree frm)))
  (unless (and (frame-selected frm) (memq (frame-selected frm) leaves))
    (when (pair? leaves)
      (set-frame-selected! frm (car leaves))))
  frm)

;; ============================================================
;; Query
;; ============================================================

(define (leaf-geometry frm lf)
  (hash-ref (frame-geometry frm) lf (λ () #f)))

(define (leaf-selected? frm lf)
  (eq? lf (frame-selected frm)))

(define (frame-leaf-list frm)
  (focus-list (frame-tree frm)))

;; ============================================================
;; Point
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
;; Tree operations
;; ============================================================

(define (replace-in-tree tree target replacement)
  (cond [(eq? tree target) replacement]
        [(split? tree)
         (split (split-direction tree)
                (map (λ (c) (replace-in-tree c target replacement))
                     (split-children tree)))]
        [else tree]))

(define (remove-from-tree tree target)
  (match tree
    [(? leaf?) (if (eq? tree target) 'removed tree)]
    [(split dir children)
     (define new-children
       (filter (λ (c) (not (eq? c 'removed)))
               (map (λ (c) (remove-from-tree c target)) children)))
     (match new-children
       ['() 'removed]
       [(list only) only]
       [_ (split dir new-children)])]))

(define (split-leaf! frm lf buf direction)
  (define new (make-leaf buf))
  (define inner (split direction (list lf new)))
  (set-frame-tree! frm (replace-in-tree (frame-tree frm) lf inner))
  (layout-frame! frm)
  new)

(define (delete-leaf! frm lf)
  (define new-tree (remove-from-tree (frame-tree frm) lf))
  (when (leaf? new-tree)
    (set! new-tree (split 'vertical (list new-tree))))
  (set-frame-tree! frm new-tree)
  (layout-frame! frm))

(define (set-leaf-buffer-in-tree! frm lf buf)
  (set-leaf-buffer! lf buf)
  (define tx (buffer-text buf))
  (set-leaf-start! lf (text-marker! tx 0 #f))
  (set-leaf-pointm! lf (buffer-point-marker buf))
  (set-leaf-hscroll! lf 0))
