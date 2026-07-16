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
   [selected? #:mutable])
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
  (window #f '() #f 0 0 0 0 buf start-m pt-m 0 #f))

(define (make-internal-window horizontal?)
  (window #f '() horizontal? 0 0 0 0 #f #f #f 0 #f))
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
