#lang racket
;; test/kernel/event-chain/chain-test.rkt
(require rackunit "../../../kernel/event-chain/chain.rkt")

(test-case "make-layer starts empty"
  (define l (make-layer))
  (check-false (unbox l)))

(test-case "push-node! adds handler to layer"
  (define l (make-layer))
  (define called? #f)
  (push-node! l (λ (evt) (set! called? #t) #f))
  (dispatch-layer (unbox l) 'some-event)
  (check-true called?))

(test-case "dispatch-layer passes event through nodes"
  (define l (make-layer))
  (push-node! l (λ (e) (string-append e "+A")))
  (push-node! l (λ (e) (string-append e "+B")))
  (check-equal? (dispatch-layer (unbox l) "start") "start+B+A"))

(test-case "dispatch-layer: #f stops chain"
  (define l (make-layer))
  (define second-called? #f)
  (push-node! l (λ (e) (set! second-called? #t) e))
  (push-node! l (λ (e) #f))  ;; consume event
  (check-false (dispatch-layer (unbox l) 'evt))
  (check-false second-called?))

(test-case "pop-node! removes handler"
  (define l (make-layer))
  (push-node! l (λ (e) "first"))
  (push-node! l (λ (e) "second"))
  (pop-node! l)
  (check-equal? (dispatch-layer (unbox l) 'x) "first"))

(test-case "push-event-handler! and dispatch-event!"
  (parameterize ([current-layers (list (make-layer))])
    (push-event-handler! (λ (e) #f))  ;; consumes event
    (check-false (dispatch-event! 'hello))))

(test-case "dispatch-event! returns event when no handler consumes"
  (parameterize ([current-layers (list (make-layer))])
    (check-equal? (dispatch-event! 'hello) 'hello)))

(test-case "pop-event-handler!"
  (parameterize ([current-layers (list (make-layer))])
    (push-event-handler! (λ (e) #f))
    (pop-event-handler!)
    (check-equal? (dispatch-event! 'hello) 'hello)))
