#lang racket

;; display/window.rkt — Window tree: leaf/split + pure layout + geometry
;;
;; Manages the viewport tree.  A leaf is a viewport into one buffer;
;; splits divide screen space.  Layout calculation is a pure function,
;; exported for user customisation.
;;
;; This module knows NOTHING about rendering, keymaps, or events.
;; It only answers: which buffer region goes to which screen rectangle.
;;
;; Architecture:
;;   leaf          — identity + viewport state (buffer, markers)
;;   split         — tree node (direction + children)
;;   rect          — screen geometry
;;   frame         — tree + computed rects + selected leaf + dims
;;
;;   layout-calc   — tree × w × h → hash[leaf→rect]  (pure, customisable)
;;   layout-frame! — apply layout-calc + write geometry  (apply)
;;
;; Dependencies: kernel/buffer, kernel/data/text, kernel/data/marker

(require "../kernel/buffer.rkt"
         "../kernel/data/text.rkt"
         "../kernel/data/marker.rkt")

(provide
 ;; ── types ──
 leaf? leaf leaf-buffer leaf-start leaf-point leaf-hscroll
 split? split split-direction split-children
 rect? rect rect-top rect-left rect-rows rect-cols
 frame? frame-tree frame-rects frame-selected frame-w frame-h

 ;; ── leaf mutation ──
 set-leaf-buffer! set-leaf-start! set-leaf-point! set-leaf-hscroll!

 ;; ── split mutation ──
 set-split-children!

 ;; ── frame mutation ──
 set-frame-tree! set-frame-rects! set-frame-selected!
 set-frame-w! set-frame-h!

 ;; ── constructors ──
 make-leaf make-rect make-frame

 ;; ── pure calc (customisable) ──
 layout-calc
 focus-list next-leaf prev-leaf leaf-geometry leaf-at-xy leaf-count

 ;; ── apply (side effects on frame) ──
 layout-frame! init-frame
 frame-split-leaf! frame-delete-leaf! frame-delete-other-leaves!
 frame-select! frame-select-next! frame-select-prev!
 frame-resize!
 leaf-set-buffer!)

;; ============================================================
;; Types
;; ============================================================

(struct leaf
  ([buffer #:mutable]    ; buffer? — the buffer this viewport shows
   [start #:mutable]     ; marker? — scroll anchor (first visible byte)
   [point #:mutable]     ; marker? — per-window cursor
   [hscroll #:mutable])  ; exact-nonnegative-integer?
  #:transparent)

(struct split
  (direction              ; 'horizontal | 'vertical
   [children #:mutable])  ; (listof (or/c leaf? split?))
  #:transparent)

(struct rect
  (top   ; exact-nonnegative-integer?
   left  ; exact-nonnegative-integer?
   rows  ; exact-positive-integer?
   cols) ; exact-positive-integer?
  #:transparent)

(struct frame
  ([tree #:mutable]      ; (or/c leaf? split?) — root of window tree
   [rects #:mutable]     ; (hash/c leaf? rect?) — computed geometry
   [selected #:mutable]  ; leaf? — currently focused leaf
   [w #:mutable]         ; exact-positive-integer? — terminal width
   [h #:mutable])        ; exact-positive-integer? — terminal height
  #:transparent)

(define (make-rect top left rows cols)
  (rect top left rows cols))

;; ============================================================
;; Constructors
;; ============================================================

(define (make-leaf buf)
  ;; Create a leaf viewing BUF.  Allocates fresh start/point markers
  ;; in the buffer's text.  start = point = 0 initially.
  (define tx (buffer-text buf))
  (leaf buf
        (text-marker! tx 0 #f)   ; start: insertion-type = #f
        (text-marker! tx 0 #t)   ; point: insertion-type = #t (stay after inserts)
        0))

(define (make-frame buf w h)
  ;; Create a frame with a single leaf viewing BUF.
  (define lf (make-leaf buf))
  (define frm (frame lf (make-hasheq) lf w h))
  (layout-frame! frm)
  frm)

(define (init-frame buf w h)
  ;; Convenience: create frame + return it.  Wraps make-frame.
  (make-frame buf w h))

;; ============================================================
;; Pure: focus-list — DFS left-to-right leaf order
;; ============================================================

(define (focus-list tree)
  (let dfs ([t tree])
    (match t
      [(? leaf?)  (list t)]
      [(split _ children)  (append-map dfs children)])))

(define (leaf-count frame-or-tree)
  (define t (if (frame? frame-or-tree) (frame-tree frame-or-tree) frame-or-tree))
  (length (focus-list t)))

;; ============================================================
;; Pure: next-leaf / prev-leaf — wrap-around focus cycling
;; ============================================================

(define (next-leaf frm)
  (define leaves (focus-list (frame-tree frm)))
  (define sel (frame-selected frm))
  (define idx (index-of leaves sel))
  (list-ref leaves (modulo (add1 idx) (length leaves))))

(define (prev-leaf frm)
  (define leaves (focus-list (frame-tree frm)))
  (define sel (frame-selected frm))
  (define idx (index-of leaves sel))
  (list-ref leaves (modulo (sub1 idx) (length leaves))))

(define (index-of lst item)
  (let loop ([xs lst] [i 0])
    (cond [(null? xs) 0]
          [(eq? (car xs) item) i]
          [else (loop (cdr xs) (add1 i))])))

;; ============================================================
;; Pure: layout-calc — tree × w × h → hash[leaf→rect]
;; ============================================================
;; Recursively split screen space.  Equal division among siblings.
;; User can replace this with a custom function of the same signature.

(define (layout-calc tree w h)
  (let loop ([t tree] [top 0] [left 0] [rows h] [cols w])
    (match t
      [(? leaf?)
       (list (cons t (make-rect top left (max 1 rows) (max 1 cols))))]
      [(split dir children)
       (define n (length children))
       (if (zero? n)
           '()
           (if (eq? dir 'vertical)
               ;; Stack vertically: share height, split rows
               (let sub ([cs children] [y top] [acc '()])
                 (if (null? cs)
                     (reverse acc)
                     (let* ([c (car cs)]
                            [remaining (- (+ top rows) y)]
                            [c-rows (max 1 (quotient remaining (length cs)))]
                            [result (loop c y left c-rows cols)])
                       (sub (cdr cs) (+ y c-rows) (append result acc)))))
               ;; Stack horizontally: share width, split cols
               (let sub ([cs children] [x left] [acc '()])
                 (if (null? cs)
                     (reverse acc)
                     (let* ([c (car cs)]
                            [remaining (- (+ left cols) x)]
                            [c-cols (max 1 (quotient remaining (length cs)))]
                            [result (loop c top x rows c-cols)])
                       (sub (cdr cs) (+ x c-cols) (append result acc)))))))])))

;; ============================================================
;; Apply: layout-frame! — run layout-calc, write geometry to frame
;; ============================================================

(define (layout-frame! frm)
  (define alist (layout-calc (frame-tree frm) (frame-w frm) (frame-h frm)))
  (define geo (make-hasheq))
  (for ([p (in-list alist)])
    (hash-set! geo (car p) (cdr p)))
  (set-frame-rects! frm geo)
  ;; If selected is gone, pick first leaf
  (define leaves (focus-list (frame-tree frm)))
  (unless (and (frame-selected frm) (memq (frame-selected frm) leaves))
    (when (pair? leaves)
      (set-frame-selected! frm (car leaves))))
  frm)

;; ============================================================
;; Pure: leaf-geometry — frame × leaf → (or rect? #f)
;; ============================================================

(define (leaf-geometry frm lf)
  (hash-ref (frame-rects frm) lf (λ () #f)))

;; ============================================================
;; Pure: leaf-at-xy — frame × x × y → (or leaf? #f)
;; ============================================================

(define (leaf-at-xy frm x y)
  (for/or ([(lf r) (in-hash (frame-rects frm))])
    (and (>= y (rect-top r))  (< y (+ (rect-top r) (rect-rows r)))
         (>= x (rect-left r)) (< x (+ (rect-left r) (rect-cols r)))
         lf)))

;; ============================================================
;; Mutations: tree structure
;; ============================================================

(define (replace-in-tree tree target replacement)
  (cond [(eq? tree target) replacement]
        [(split? tree)
         (split (split-direction tree)
                (map (λ (c) (replace-in-tree c target replacement))
                     (split-children tree)))]
        [else tree]))

(define (remove-from-tree tree target)
  ;; Returns the new tree, or the symbol 'removed if tree was the target.
  (match tree
    [(? leaf?)  (if (eq? tree target) 'removed tree)]
    [(split dir children)
     (define new-children
       (filter (λ (c) (not (eq? c 'removed)))
               (map (λ (c) (remove-from-tree c target)) children)))
     (match new-children
       ['()       'removed]
       [(list c)  c]               ; collapse single-child split
       [_         (split dir new-children)])]))

;; ============================================================
;; frame-split-leaf! — split the selected leaf, creating a new one
;; ============================================================

(define (frame-split-leaf! frm direction)
  ;; Splits the space occupied by the selected leaf.
  ;; The new leaf shows the same buffer as the original.
  ;; Returns the new leaf.
  (define old (frame-selected frm))
  (define new (make-leaf (leaf-buffer old)))
  (define inner (split direction (list old new)))
  (set-frame-tree! frm (replace-in-tree (frame-tree frm) old inner))
  (layout-frame! frm)
  ;; Select the new leaf
  (set-frame-selected! frm new)
  new)

;; ============================================================
;; frame-delete-leaf! — remove a leaf, preserving at least one
;; ============================================================

(define (frame-delete-leaf! frm)
  ;; Delete the selected leaf.  Cannot delete the last leaf.
  (define sel (frame-selected frm))
  (define leaves (focus-list (frame-tree frm)))
  (when (> (length leaves) 1)
    (define new-tree (remove-from-tree (frame-tree frm) sel))
    (when (leaf? new-tree)
      ;; After collapse, wrap in a split to keep tree valid
      (set! new-tree (split 'vertical (list new-tree))))
    (set-frame-tree! frm new-tree)
    (layout-frame! frm)))

;; ============================================================
;; frame-delete-other-leaves! — keep only the selected leaf
;; ============================================================

(define (frame-delete-other-leaves! frm)
  (define sel (frame-selected frm))
  (define inner (split 'vertical (list sel)))
  (set-frame-tree! frm inner)
  (layout-frame! frm)
  (set-frame-selected! frm sel))

;; ============================================================
;; Focus operations
;; ============================================================

(define (frame-select! frm lf)
  (when (memq lf (focus-list (frame-tree frm)))
    (set-frame-selected! frm lf)))

(define (frame-select-next! frm)
  (set-frame-selected! frm (next-leaf frm)))

(define (frame-select-prev! frm)
  (set-frame-selected! frm (prev-leaf frm)))

;; ============================================================
;; frame-resize! — update dimensions + relayout
;; ============================================================

(define (frame-resize! frm w h)
  (set-frame-w! frm w)
  (set-frame-h! frm h)
  (layout-frame! frm))

;; ============================================================
;; leaf-set-buffer! — change what buffer a leaf views
;; ============================================================

(define (leaf-set-buffer! lf buf)
  (set-leaf-buffer! lf buf)
  (define tx (buffer-text buf))
  ;; Fresh markers at buffer start
  (set-leaf-start! lf (text-marker! tx 0 #f))
  (set-leaf-point! lf (text-marker! tx 0 #t))
  (set-leaf-hscroll! lf 0))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)

  (define (test-buf [s "hello world"])
    (make-buffer "test" s))

  (test-case "make-leaf creates markers"
    (define buf (test-buf))
    (define lf (make-leaf buf))
    (check-eq? (leaf-buffer lf) buf)
    (check-equal? (marker-pos (leaf-start lf)) 0)
    (check-equal? (marker-pos (leaf-point lf)) 0)
    (check-equal? (leaf-hscroll lf) 0))

  (test-case "make-frame with single leaf"
    (define frm (make-frame (test-buf) 80 24))
    (check-equal? (frame-w frm) 80)
    (check-equal? (frame-h frm) 24)
    (check-equal? (leaf-count frm) 1)
    (check-true (leaf? (frame-selected frm))))

  (test-case "layout-calc: single leaf fills screen"
    (define lf (make-leaf (test-buf)))
    (define result (layout-calc lf 80 24))
    (check-equal? (length result) 1)
    (match-define (list (cons lf* r)) result)
    (check-eq? lf* lf)
    (check-equal? (rect-top r) 0)
    (check-equal? (rect-left r) 0)
    (check-equal? (rect-rows r) 24)
    (check-equal? (rect-cols r) 80))

  (test-case "layout-calc: vertical split"
    (define a (make-leaf (test-buf "aaa")))
    (define b (make-leaf (test-buf "bbb")))
    (define tree (split 'vertical (list a b)))
    (define result (layout-calc tree 80 24))
    (define geo (make-hash))
    (for ([p (in-list result)]) (hash-set! geo (car p) (cdr p)))
    (define ra (hash-ref geo a))
    (define rb (hash-ref geo b))
    (check-equal? (rect-top ra) 0)
    (check-equal? (rect-rows ra) 12)
    (check-equal? (rect-top rb) 12)
    (check-equal? (rect-rows rb) 12)
    ;; Both full width
    (check-equal? (rect-left ra) 0)  (check-equal? (rect-cols ra) 80)
    (check-equal? (rect-left rb) 0)  (check-equal? (rect-cols rb) 80))

  (test-case "layout-calc: horizontal split"
    (define a (make-leaf (test-buf "aaa")))
    (define b (make-leaf (test-buf "bbb")))
    (define tree (split 'horizontal (list a b)))
    (define result (layout-calc tree 80 24))
    (define geo (make-hash))
    (for ([p (in-list result)]) (hash-set! geo (car p) (cdr p)))
    (define ra (hash-ref geo a))
    (define rb (hash-ref geo b))
    (check-equal? (rect-left ra) 0)   (check-equal? (rect-cols ra) 40)
    (check-equal? (rect-left rb) 40)  (check-equal? (rect-cols rb) 40)
    ;; Both full height
    (check-equal? (rect-top ra) 0)  (check-equal? (rect-rows ra) 24)
    (check-equal? (rect-top rb) 0)  (check-equal? (rect-rows rb) 24))

  (test-case "layout-calc: nested split"
    (define a (make-leaf (test-buf "a")))
    (define b (make-leaf (test-buf "b")))
    (define c (make-leaf (test-buf "c")))
    ;; vertical: a on top, (horizontal: b left, c right) on bottom
    (define inner (split 'horizontal (list b c)))
    (define tree (split 'vertical (list a inner)))
    (define result (layout-calc tree 80 24))
    (define geo (make-hash))
    (for ([p (in-list result)]) (hash-set! geo (car p) (cdr p)))
    ;; a: top half, full width
    (check-equal? (rect-rows (hash-ref geo a)) 12)
    (check-equal? (rect-cols (hash-ref geo a)) 80)
    ;; b: bottom-left quarter
    (check-equal? (rect-top (hash-ref geo b)) 12)
    (check-equal? (rect-rows (hash-ref geo b)) 12)
    (check-equal? (rect-cols (hash-ref geo b)) 40)
    ;; c: bottom-right quarter
    (check-equal? (rect-left (hash-ref geo c)) 40))

  (test-case "focus-list order"
    (define a (make-leaf (test-buf "a")))
    (define b (make-leaf (test-buf "b")))
    (define c (make-leaf (test-buf "c")))
    (define tree (split 'vertical (list a (split 'horizontal (list b c)))))
    (define leaves (focus-list tree))
    (check-equal? (length leaves) 3)
    (check-eq? (list-ref leaves 0) a)
    (check-eq? (list-ref leaves 1) b)
    (check-eq? (list-ref leaves 2) c))

  (test-case "next-leaf / prev-leaf wrap around"
    (define frm (make-frame (test-buf) 80 24))
    (frame-split-leaf! frm 'vertical)
    (check-equal? (leaf-count frm) 2)
    (define sel (frame-selected frm))
    (define nx (next-leaf frm))
    ;; next should be different from selected
    (check-false (eq? sel nx))
    ;; prev from the current position wraps to the other leaf
    (define prev (prev-leaf frm))
    (check-false (eq? sel prev))
    ;; After two next-leaf calls (or two prev-leaf), cycle brings us back
    (frame-select-next! frm)
    (frame-select-next! frm)
    (check-eq? (frame-selected frm) sel))

  (test-case "leaf-at-xy"
    (define frm (make-frame (test-buf) 80 24))
    (define lf (frame-selected frm))
    (check-eq? (leaf-at-xy frm 5 5) lf)
    (check-false (leaf-at-xy frm 100 100)))

  (test-case "frame-split-leaf! creates new leaf with same buffer"
    (define buf (test-buf))
    (define frm (make-frame buf 80 24))
    (define new (frame-split-leaf! frm 'vertical))
    (check-eq? (leaf-buffer new) buf)
    (check-equal? (leaf-count frm) 2)
    (check-eq? (frame-selected frm) new))

  (test-case "frame-delete-leaf! cannot delete sole leaf"
    (define frm (make-frame (test-buf) 80 24))
    (frame-delete-leaf! frm)
    (check-equal? (leaf-count frm) 1))

  (test-case "frame-delete-leaf! collapses split"
    (define frm (make-frame (test-buf) 80 24))
    (frame-split-leaf! frm 'vertical)
    (check-equal? (leaf-count frm) 2)
    (frame-delete-leaf! frm)
    (check-equal? (leaf-count frm) 1))

  (test-case "frame-delete-other-leaves!"
    (define a (test-buf "aaa"))
    (define frm (make-frame a 80 24))
    (frame-split-leaf! frm 'vertical)
    (frame-split-leaf! frm 'horizontal)
    (check-equal? (leaf-count frm) 3)
    (frame-delete-other-leaves! frm)
    (check-equal? (leaf-count frm) 1))

  (test-case "frame-select-next! cycles"
    (define frm (make-frame (test-buf) 80 24))
    (frame-split-leaf! frm 'vertical)
    (frame-split-leaf! frm 'vertical)
    (check-equal? (leaf-count frm) 3)
    (define s0 (frame-selected frm))
    (frame-select-next! frm)
    (define s1 (frame-selected frm))
    (check-false (eq? s0 s1))
    (frame-select-next! frm)
    (frame-select-next! frm)
    ;; After 3 cycles should be back
    (check-eq? (frame-selected frm) s0))

  (test-case "frame-resize! re-layouts"
    (define frm (make-frame (test-buf) 80 24))
    (frame-split-leaf! frm 'vertical)
    (frame-resize! frm 100 40)
    (check-equal? (frame-w frm) 100)
    (check-equal? (frame-h frm) 40)
    (for ([lf (in-list (focus-list (frame-tree frm)))])
      (define r (leaf-geometry frm lf))
      (check-true (rect? r))
      (check-true (< (rect-top r) 40))))

  (test-case "leaf-set-buffer! changes buffer and resets markers"
    (define old-buf (test-buf "old contents here"))
    (define new-buf (test-buf "new buffer"))
    (define frm (make-frame old-buf 80 24))
    (define lf (frame-selected frm))
    ;; Move point in old buffer (within bounds)
    (set-buffer-point! old-buf 6)
    (check-equal? (buffer-point old-buf) 6)
    ;; Switch buffer
    (leaf-set-buffer! lf new-buf)
    (check-eq? (leaf-buffer lf) new-buf)
    (check-equal? (marker-pos (leaf-start lf)) 0)
    (check-equal? (marker-pos (leaf-point lf)) 0)
    (check-equal? (leaf-hscroll lf) 0))
)
