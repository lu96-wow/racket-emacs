#lang racket
;; test/kernel/vbuffer/vbuffer-test.rkt
(require rackunit "../../../kernel/vbuffer/vbuffer.rkt")

(test-case "make-vbuffer dimensions"
  (define vb (make-vbuffer 3 5))
  (check-equal? (vbuffer-rows vb) 3)
  (check-equal? (vbuffer-cols vb) 5)
  (check-equal? (vector-length (vbuffer-cells vb)) 15))

(test-case "cells default to space face-id 0"
  (define vb (make-vbuffer 1 3))
  (for ([c (in-range 3)])
    (check-equal? (cell-ch (vector-ref (vbuffer-cells vb) c)) #\space)
    (check-equal? (cell-face-id (vector-ref (vbuffer-cells vb) c)) 0)
    (check-false (cell-attrs (vector-ref (vbuffer-cells vb) c)))))

(test-case "vbuffer-put-char!"
  (define vb (make-vbuffer 2 5))
  (vbuffer-put-char! vb 0 0 #\X 'bold #:face-id 3)
  (check-equal? (cell-ch (vector-ref (vbuffer-cells vb) 0)) #\X)
  (check-equal? (cell-attrs (vector-ref (vbuffer-cells vb) 0)) 'bold)
  (check-equal? (cell-face-id (vector-ref (vbuffer-cells vb) 0)) 3))

(test-case "vbuffer-put-string!"
  (define vb (make-vbuffer 1 10))
  (vbuffer-put-string! vb 0 2 "hello")
  (check-equal? (vbuffer-row->string vb 0) "  hello   "))

(test-case "vbuffer-clear!"
  (define vb (make-vbuffer 1 3))
  (vbuffer-put-char! vb 0 1 #\X)
  (vbuffer-clear! vb)
  (check-equal? (vbuffer-row->string vb 0) "   "))

(test-case "vbuffer-blit!"
  (define dst (make-vbuffer 3 5))
  (define src (make-vbuffer 2 3))
  (vbuffer-put-string! src 0 0 "abc")
  (vbuffer-put-string! src 1 0 "def")
  (vbuffer-blit! dst 1 1 src)
  (check-equal? (vbuffer-row->string dst 0) "     ")
  (check-equal? (vbuffer-row->string dst 1) " abc ")
  (check-equal? (vbuffer-row->string dst 2) " def "))

(test-case "vbuffer->lines"
  (define vb (make-vbuffer 2 3))
  (vbuffer-put-string! vb 0 0 "ab")
  (vbuffer-put-string! vb 1 0 "cd")
  (check-equal? (vbuffer->lines vb) '("ab " "cd ")))
