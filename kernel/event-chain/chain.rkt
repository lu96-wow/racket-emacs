#lang racket

;; kernel/event-chain/chain.rkt — Layered event dispatch (node chain)
;;
;; A linked list of handler nodes.  Each handler receives an event
;; and returns either #f (consumed) or a new event (pass to next).
;; Layers are stacked; an event bubbles through all layers.

(provide
 node? node node-handler node-next set-node-next!
 make-layer push-node! pop-node! dispatch-layer
 current-layers push-event-handler! pop-event-handler!
 dispatch-event!)

;; ============================================================
;; Node
;; ============================================================

(struct node
  ([handler #:mutable]   ; event -> (or/c #f event)
   [next #:mutable])     ; (or/c node? #f)
  #:transparent)

;; ============================================================
;; Layer
;; ============================================================

(define (make-layer)
  (box #f))  ; #f or node? (head of linked list)

(define (push-node! layer handler)
  (set-box! layer (node handler (unbox layer))))

(define (pop-node! layer)
  (when (unbox layer)
    (set-box! layer (node-next (unbox layer)))))

;; ============================================================
;; Dispatch
;; ============================================================

(define (dispatch-layer head ke)
  (let loop ([n head] [evt ke])
    (match n
      [#f evt]
      [_ (let ([r ((node-handler n) evt)])
           (if r (loop (node-next n) r) #f))])))

;; ============================================================
;; Global layers
;; ============================================================

(define current-layers (make-parameter (list (make-layer))))

(define (push-event-handler! handler [layer-idx 0])
  (define layers (current-layers))
  (define padded
    (if (< layer-idx (length layers))
        layers
        (let loop ([ls layers] [n (- layer-idx (sub1 (length layers)))])
          (if (zero? n) ls (loop (append ls (list (make-layer))) (sub1 n))))))
  (push-node! (list-ref padded layer-idx) handler)
  (current-layers padded))

(define (pop-event-handler!)
  (define layers (current-layers))
  (when (and (pair? layers) (unbox (car layers)))
    (pop-node! (car layers))))

(define (dispatch-event! ke)
  (for/fold ([evt ke])
            ([layer (in-list (current-layers))])
    #:break (not evt)
    (dispatch-layer (unbox layer) evt)))
