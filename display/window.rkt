#lang racket

;; display/window.rkt — Window tree: leaf/split + pure layout + geometry
;;
;; ============================================================================
;; Manages the viewport tree.  A leaf is a viewport into one buffer;
;; splits divide screen space.  Layout calculation is a pure function.
;;
;; ============================================================================
;; Computation vs Application
;; ============================================================================
;;
;;   ── Pure Calculation ──
;;     layout-calc          tree × w × h → hash[leaf→rect]
;;     leaf-geometry        frame × leaf → rect | #f
;;     leaf-at-xy           frame × x × y (0-based) → leaf | #f
;;     focus-list           tree → (listof leaf?)
;;     next-leaf prev-leaf  frame → leaf?
;;     leaf-count           frame | tree → exact-nonnegative-integer?
;;
;;     frame-xy->leaf       frame × x × y (1-based SGR) → leaf | #f
;;     leaf-xy->buffer-pos  leaf × rect × x × y (0-based) → byte-pos | #f
;;
;;   ── Application (side effects on frame/leaf) ──
;;     layout-frame!               apply layout-calc → write frame-rects
;;     apply-scroll!               write scroll result to leaf markers
;;     frame-select!               switch focus, detach/attach point markers
;;     frame-split-leaf!           split space, create new leaf
;;     frame-select-next!/prev!    cycle focus
;;     detach-leaf-point!           give leaf independent marker
;;     attach-leaf-point!           share buffer's point-marker
;;
;; ============================================================================
;; Dependencies
;; ============================================================================
;;
;;   kernel/buffer.rkt        — buffer struct + point/mark operations
;;   kernel/data/text.rkt     — text-gap, text-marker!
;;   kernel/data/marker.rkt   — marker-pos, set-marker-pos!
;;   display/layout.rkt       — compute-layout, calc-scroll
;; ============================================================================

(require "../kernel/buffer.rkt"
         "../kernel/data/text.rkt"
         "../kernel/data/marker.rkt"
         "layout.rkt")

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
 make-leaf make-rect make-frame init-frame

 ;; ── pure queries ──
 layout-calc
 focus-list next-leaf prev-leaf leaf-geometry leaf-at-xy leaf-count

 ;; ── screen → buffer ──
 frame-xy->leaf      ;; frame × x × y (1-based SGR) → leaf | #f
 leaf-xy->buffer-pos ;; leaf × rect × x × y (0-based local) → byte-pos | #f
 frame-point-to-xy!  ;; frame × x × y → boolean? (moves point, switches focus)

 ;; ── scroll ──
 apply-scroll!

 ;; ── per-leaf point management ──
 detach-leaf-point!  ;; leaf → independent marker
 attach-leaf-point!   ;; leaf → share buffer-point-marker

 ;; ── apply (side effects on frame) ──
 layout-frame!
 frame-split-leaf! frame-delete-leaf! frame-delete-other-leaves!
 frame-select! frame-select-next! frame-select-prev!
 frame-resize! frame-resize
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
  (unless (and (exact-nonnegative-integer? top)
               (exact-nonnegative-integer? left)
               (exact-positive-integer? rows)
               (exact-positive-integer? cols))
    (raise-argument-error 'make-rect
                          "non-negative top/left, positive rows/cols"
                          (list top left rows cols)))
  (rect top left rows cols))

;; ============================================================
;; Constructors
;; ============================================================

(define (make-leaf buf)
  ;; Create a leaf viewing BUF.
  ;; The point marker IS buffer-point-marker — mutations to buffer-point
  ;; directly affect the selected leaf's cursor.
  (unless (buffer? buf)
    (raise-argument-error 'make-leaf "buffer?" buf))
  (define tx (buffer-text buf))
  (leaf buf
        (text-marker! tx 0 #f)               ;; start: insertion-type = #f
        (buffer-point-marker buf)             ;; point: shares buffer's marker
        0))

(define (make-frame buf w h)
  ;; Create a frame with a single leaf viewing BUF.
  (unless (buffer? buf)
    (raise-argument-error 'make-frame "buffer?" buf))
  (define lf (make-leaf buf))
  (define frm (frame lf (make-hasheq) lf w h))
  (layout-frame! frm)
  frm)

(define (init-frame buf w h)
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

(define (layout-calc tree w h)
  ;; Recursively split screen space.  Equal division among siblings.
  ;; Customisable: replace with any function of the same signature.
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
;; Pure: leaf-at-xy — frame × x × y (0-based) → (or leaf? #f)
;; ============================================================

(define (leaf-at-xy frm x y)
  (for/or ([(lf r) (in-hash (frame-rects frm))])
    (and (>= y (rect-top r))  (< y (+ (rect-top r) (rect-rows r)))
         (>= x (rect-left r)) (< x (+ (rect-left r) (rect-cols r)))
         lf)))

;; ============================================================
;; frame-xy->leaf — terminal (x,y) (1-based SGR) → leaf
;; ============================================================

(define (frame-xy->leaf frm x y)
  ;; x, y : exact-positive-integer? — 1-based from SGR mouse
  ;; → (or/c leaf? #f)
  (leaf-at-xy frm (sub1 x) (sub1 y)))

;; ============================================================
;; leaf-xy->buffer-pos — leaf-local (x,y) (0-based) → buffer byte-pos
;; ============================================================

(define (leaf-xy->buffer-pos lf geo local-x local-y)
  ;; Given a leaf and local coordinates (relative to leaf rect origin),
  ;; compute the buffer byte position.  Returns (or/c byte-pos? #f).
  (define buf (leaf-buffer lf))
  (define gb  (text-gap (buffer-text buf)))
  (define pt  (marker-pos (leaf-point lf)))
  (define ws  (marker-pos (leaf-start lf)))
  (define rows (rect-rows geo))
  (define cols (rect-cols geo))
  (define hs  (leaf-hscroll lf))

  ;; Recompute layout from leaf state + rect geometry.
  (define ly (compute-layout gb pt
               #:start-pos ws
               #:max-rows  rows
               #:max-cols  cols
               #:wrap-mode 'none
               #:left-col  hs))

  (layout-query-pos gb ly local-y local-x))

(define (frame-point-to-xy! frm x y)
  ;; Move point to the buffer position at terminal (x,y).
  ;; x, y : exact-positive-integer? — 1-based SGR coordinates.
  ;; Returns boolean? — #t if point moved, #f if outside all leaves.
  (define lf (frame-xy->leaf frm x y))
  (and lf
       (let* (;; Switch focus if needed
              [_ (when (not (eq? lf (frame-selected frm)))
                   (frame-select! frm lf))]
              [geo     (leaf-geometry frm lf)]
              [local-x (and geo (- (sub1 x) (rect-left geo)))]
              [local-y (and geo (- (sub1 y) (rect-top geo)))]
              [buf-pos (and geo (leaf-xy->buffer-pos lf geo local-x local-y))])
         (and buf-pos
              (begin
                (set-buffer-point! (leaf-buffer lf) buf-pos)
                #t)))))

;; ============================================================
;; Tree structure mutations
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
       [(list c)  c]               ;; collapse single-child split
       [_         (split dir new-children)])]))

;; ============================================================
;; frame-split-leaf! — split the selected leaf
;; ============================================================

(define (frame-split-leaf! frm direction)
  ;; Splits the space occupied by the selected leaf.
  ;; The new leaf shows the same buffer.
  (define old (frame-selected frm))
  (define new (make-leaf (leaf-buffer old)))
  (define inner (split direction (list old new)))
  (set-frame-tree! frm (replace-in-tree (frame-tree frm) old inner))
  (layout-frame! frm)
  (frame-select! frm new)
  new)

;; ============================================================
;; frame-delete-leaf! — remove a leaf, preserving at least one
;; ============================================================

(define (frame-delete-leaf! frm)
  (define sel (frame-selected frm))
  (define leaves (focus-list (frame-tree frm)))
  (when (> (length leaves) 1)
    (define new-tree (remove-from-tree (frame-tree frm) sel))
    (when (leaf? new-tree)
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
;; detach-leaf-point! — give leaf its own independent marker
;; ============================================================

(define (detach-leaf-point! lf)
  ;; Called when a leaf LOSES focus.  Allocates a fresh marker
  ;; at the current buffer-point position.
  (define buf (leaf-buffer lf))
  (define tx  (buffer-text buf))
  (define old-pos (text-marker-pos tx (leaf-point lf)))
  (set-leaf-point! lf (text-marker! tx old-pos #t)))

;; ============================================================
;; attach-leaf-point! — share buffer's point-marker
;; ============================================================

(define (attach-leaf-point! lf)
  ;; Called when a leaf GAINS focus.  Moves buffer-point to this
  ;; leaf's saved cursor position, then shares the marker.
  (define buf (leaf-buffer lf))
  (define tx  (buffer-text buf))
  (text-set-marker-pos! tx (buffer-point-marker buf)
                         (text-marker-pos tx (leaf-point lf)))
  (set-leaf-point! lf (buffer-point-marker buf)))

;; ============================================================
;; Focus operations
;; ============================================================

(define (frame-select! frm lf)
  (when (and (memq lf (focus-list (frame-tree frm)))
             (not (eq? lf (frame-selected frm))))
    (when (frame-selected frm)
      (detach-leaf-point! (frame-selected frm)))
    (attach-leaf-point! lf)
    (set-frame-selected! frm lf)))

(define (frame-select-next! frm)
  (frame-select! frm (next-leaf frm)))

(define (frame-select-prev! frm)
  (frame-select! frm (prev-leaf frm)))

;; ============================================================
;; frame-resize! / frame-resize
;; ============================================================

(define (frame-resize! frm w h)
  (set-frame-w! frm w)
  (set-frame-h! frm h)
  (layout-frame! frm))

(define (frame-resize frm w h)
  ;; Pure: returns new frame (triggers cache invalidation in caller).
  (define new-frm (struct-copy frame frm [w w] [h h]))
  (layout-frame! new-frm)
  new-frm)

;; ============================================================
;; apply-scroll! — write scroll result to leaf markers
;; ============================================================

(define (apply-scroll! lf new-start new-hscroll)
  ;; Pure composition: take leaf + calc-scroll result → update markers.
  ;; Returns the leaf (mutated in place).
  (define buf (leaf-buffer lf))
  (define tx  (buffer-text buf))
  (text-set-marker-pos! tx (leaf-start lf) new-start)
  (set-leaf-hscroll! lf new-hscroll)
  lf)

;; ============================================================
;; leaf-set-buffer! — change what buffer a leaf views
;; ============================================================

(define (leaf-set-buffer! lf buf)
  (unless (buffer? buf)
    (raise-argument-error 'leaf-set-buffer! "buffer?" buf))
  (set-leaf-buffer! lf buf)
  (define tx (buffer-text buf))
  (set-leaf-start! lf (text-marker! tx 0 #f))
  (set-leaf-point! lf (text-marker! tx 0 #t))
  (set-leaf-hscroll! lf 0))
