#lang racket

;; core/window.rkt — Window & frame primitives (struct + layout)

(require "../kernel/buffer.rkt"
         "../kernel/marker.rkt")

(provide
 ;; window
 window? window-parent window-children window-horizontal?
 window-top window-left window-rows window-cols
 window-hscroll set-window-hscroll!
 window-buffer set-window-buffer! window-start window-pointm window-selected? window-mini?
 set-window-parent! set-window-children! set-window-horizontal?!
 set-window-top! set-window-left! set-window-rows! set-window-cols!
 set-window-start! set-window-pointm!
 set-window-selected?! set-window-mini?!

 ;; frame
 frame? frame-root-window frame-minibuffer-window frame-selected-window
 frame-width frame-height
 set-frame-root-window! set-frame-minibuffer-window! set-frame-selected-window!
 set-frame-width! set-frame-height!

 ;; query
 window-leaf? window-live-p
 frame-window-list
 selected-window current-frame

 ;; create
 make-leaf-window make-internal-window make-minibuffer-window
 init-root-frame

 ;; layout
 layout-frame!
 window-desired-rows set-window-desired-rows!

 ;; point
 window-point set-window-point!)

;; ============================================================
;; Window
;; ============================================================

(struct window
  ([parent #:mutable] [children #:mutable] [horizontal? #:mutable]
   [top #:mutable] [left #:mutable] [rows #:mutable] [cols #:mutable]
   [buffer #:mutable] [start #:mutable] [pointm #:mutable] [hscroll #:mutable]
   [selected? #:mutable] [mini? #:mutable])
  #:transparent)

;; ============================================================
;; Frame
;; ============================================================

(struct frame
  ([root-window #:mutable] [minibuffer-window #:mutable]
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
  (define mini (frame-minibuffer-window frm))
  (when (and mini (window-live-p mini)) (set! acc (cons mini acc)))
  (reverse acc))

(define current-frame (make-parameter #f))
(define (selected-window) (define frm (current-frame)) (and frm (frame-selected-window frm)))

;; ============================================================
;; Constructors
;; ============================================================

(define (make-leaf-window buf)
  (define start-m (make-marker 0 #f buf))
  (define pt-m    (make-marker 0 #t buf))
  (set-buffer-markers! buf (cons start-m (cons pt-m (buffer-markers buf))))
  (window #f '() #f 0 0 0 0 buf start-m pt-m 0 #f #f))

(define (make-internal-window horizontal?)
  (window #f '() horizontal? 0 0 0 0 #f #f #f 0 #f #f))

(define (make-minibuffer-window buf)
  (define pt-m (make-marker 0 #t buf))
  (set-buffer-markers! buf (cons pt-m (buffer-markers buf)))
  (window #f '() #f 0 0 0 0 buf #f pt-m 0 #f #t))

(define (window-point w)
  (cond [(window-mini? w) (marker-pos (window-pointm w))]
        [(window-leaf? w) (marker-pos (window-pointm w))]
        [else 0]))
(define (set-window-point! w pos)
  (when (window-leaf? w) (set-marker-pos! (window-pointm w) pos)))

;; ============================================================
;; Layout
;; ============================================================

(define window-desired-rows-table (make-hasheq))
(define (window-desired-rows w) (hash-ref window-desired-rows-table w (λ () 1)))
(define (set-window-desired-rows! w rows) (hash-set! window-desired-rows-table w rows))

(define (init-root-frame main-buf mini-buf width height)
  (define root (make-leaf-window main-buf))
  (set-window-selected?! root #t)
  (define mini (make-minibuffer-window mini-buf))
  (define frm (frame root mini root width height))
  (layout-frame! frm)
  (current-frame frm)
  frm)

(define (layout-frame! frm)
  (define fw (frame-width frm)) (define fh (frame-height frm))
  (define mini (frame-minibuffer-window frm)) (define root (frame-root-window frm))
  (when mini
    (define mini-rows (window-desired-rows mini))
    (set-window-top! mini (- fh mini-rows)) (set-window-left! mini 0)
    (set-window-rows! mini mini-rows) (set-window-cols! mini fw))
  (define root-rows (- fh (if mini (window-desired-rows mini) 0)))
  (layout-subtree! root 0 0 root-rows fw))

(define (layout-subtree! w top left rows cols)
  (set-window-top! w top) (set-window-left! w left)
  (set-window-rows! w rows) (set-window-cols! w cols)
  (unless (null? (window-children w))
    (define child-count (length (window-children w)))
    (if (window-horizontal? w)
        (let loop ([children (window-children w)] [l left])
          (unless (null? children)
            (define child (car children)) (define child-cols (quotient cols child-count))
            (layout-subtree! child top l rows child-cols) (loop (cdr children) (+ l child-cols))))
        (let loop ([children (window-children w)] [t top])
          (unless (null? children)
            (define child (car children)) (define child-rows (quotient rows child-count))
            (layout-subtree! child t left child-rows cols) (loop (cdr children) (+ t child-rows)))))))
