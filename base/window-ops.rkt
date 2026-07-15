#lang racket

;; base/window-ops.rkt — Window operations (composed from kernel primitives)

(require "../core/window.rkt"
         "../kernel/buffer.rkt"
         "../kernel/marker.rkt")

(provide
 split-window-below split-window-right
 delete-window delete-other-windows other-window
 switch-buffer-in-window!)

(define (split-window-below #:size [size #f]) (split-window #f size))
(define (split-window-right #:size [size #f]) (split-window #t size))

(define (split-window horizontal? size)
  (define frm (current-frame))
  (define sel (frame-selected-window frm))
  (define new-buf (window-buffer sel)) (define new-win (make-leaf-window new-buf))
  (define parent (window-parent sel))
  (cond [(not parent) (let ([internal (make-internal-window horizontal?)])
                        (set-window-children! internal (list sel new-win))
                        (set-window-parent! sel internal) (set-window-parent! new-win internal)
                        (set-frame-root-window! frm internal))]
        [(window-horizontal? parent) (let ([internal (make-internal-window horizontal?)])
                                       (set-window-children! internal (list sel new-win))
                                       (set-window-parent! new-win internal)
                                       (set-window-children! parent (substitute sel internal (window-children parent)))
                                       (set-window-parent! sel internal) (set-window-parent! internal parent))]
        [else (if (eq? horizontal? #f)
                  (begin (set-window-children! parent (insert-after sel new-win (window-children parent)))
                         (set-window-parent! new-win parent))
                  (let ([internal (make-internal-window horizontal?)])
                    (set-window-children! internal (list sel new-win)) (set-window-parent! new-win internal)
                    (set-window-children! parent (substitute sel internal (window-children parent)))
                    (set-window-parent! sel internal) (set-window-parent! internal parent)))])
  (layout-frame! frm) (set-window-selected?! sel #f) (set-window-selected?! new-win #t)
  (set-frame-selected-window! frm new-win) new-win)

(define (substitute old new lst) (for/list ([x lst]) (if (eq? x old) new x)))
(define (insert-after target item lst)
  (define out '()) (for ([x lst]) (set! out (cons x out)) (when (eq? x target) (set! out (cons item out)))) (reverse out))
(define (index-of lst item)
  (let loop ([lst lst] [i 0]) (cond [(null? lst) #f] [(eq? (car lst) item) i] [else (loop (cdr lst) (add1 i))])))

(define (delete-window #:window [win #f])
  (define frm (current-frame)) (define w (or win (frame-selected-window frm)))
  (define all-leaves (filter (λ (x) (and (window-leaf? x) (not (window-mini? x)))) (frame-window-list frm)))
  (define parent (window-parent w))
  (define new-selected (or (window-prev-sibling w) (window-next-sibling w) (and parent (car (window-children parent))) (car all-leaves)))
  (cond [(not parent) (define other (findf (λ (x) (and (window-leaf? x) (not (eq? x w)))) (frame-window-list frm)))
         (when other (set-frame-root-window! frm other) (set-window-parent! other #f) (detach! w))]
        [else (define new-children (remove w (window-children parent)))
         (if (= (length new-children) 1)
             (let ([survivor (car new-children)]) (define grandparent (window-parent parent))
               (if grandparent (begin (set-window-children! grandparent (substitute parent survivor (window-children grandparent)))
                                      (set-window-parent! survivor grandparent))
                   (begin (set-frame-root-window! frm survivor) (set-window-parent! survivor #f))))
             (set-window-children! parent new-children)) (detach! w)])
  (layout-frame! frm) (when new-selected (set-window-selected?! new-selected #t) (set-frame-selected-window! frm new-selected)
                        (define buf (window-buffer new-selected)) (when buf (set-buffer buf))) new-selected)

(define (detach! w) (define start-m (window-start w)) (define pt-m (window-pointm w))
  (when start-m (set-marker-buffer! start-m #f)) (when pt-m (set-marker-buffer! pt-m #f)))

(define (delete-other-windows)
  (define frm (current-frame)) (define sel (frame-selected-window frm))
  (for ([w (in-list (filter window-leaf? (frame-window-list frm)))]
        #:when (and (not (eq? w sel)) (not (window-mini? w)))) (detach! w))
  (set-window-parent! sel #f) (set-window-children! sel '()) (set-window-horizontal?! sel #f)
  (set-frame-root-window! frm sel) (layout-frame! frm) sel)

(define (other-window [count 1])
  (define frm (current-frame)) (define leaves (filter (λ (w) (and (window-leaf? w) (not (window-mini? w)))) (frame-window-list frm)))
  (define sel (frame-selected-window frm)) (define idx (or (index-of leaves sel) 0))
  (define next-win (list-ref leaves (modulo (+ idx count) (length leaves))))
  (when (and sel (window-leaf? sel) (not (window-mini? sel))) (define buf (window-buffer sel))
    (when buf (set-marker-pos! (window-pointm sel) (buffer-point buf))))
  (when (window-leaf? sel) (set-window-selected?! sel #f)) (set-window-selected?! next-win #t)
  (set-frame-selected-window! frm next-win) (define next-buf (window-buffer next-win))
  (when next-buf (set-buffer next-buf) (set-buffer-point! next-buf (window-point next-win))) next-win)

(define (switch-buffer-in-window! w buf)
  (define old-buf (window-buffer w))
  (when (and old-buf (not (eq? old-buf buf)) (not (window-mini? w)))
    (set-marker-pos! (window-pointm w) (marker-pos (window-pointm w))))
  (set-window-buffer! w buf) (define start-m (make-marker (if (window-mini? w) 0 (buffer-point buf)) #f buf))
  (define pt-m (make-marker (buffer-point buf) #t buf))
  (set-buffer-markers! buf (cons start-m (cons pt-m (buffer-markers buf))))
  (when (window-start w) (set-marker-buffer! (window-start w) #f))
  (when (window-pointm w) (set-marker-buffer! (window-pointm w) #f))
  (set-window-start! w start-m) (set-window-pointm! w pt-m)
  (when (eq? w (selected-window)) (set-buffer buf)) buf)

(define window-next-sibling
  (let () (define (f w) (define p (window-parent w))
            (and p (let* ([sibs (window-children p)] [idx (index-of sibs w)])
                     (and idx (< (add1 idx) (length sibs)) (list-ref sibs (add1 idx)))))) f))
(define window-prev-sibling
  (let () (define (f w) (define p (window-parent w))
            (and p (let* ([sibs (window-children p)] [idx (index-of sibs w)])
                     (and idx (> idx 0) (list-ref sibs (sub1 idx)))))) f))
