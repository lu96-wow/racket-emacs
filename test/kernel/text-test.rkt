#lang racket
;; test/kernel/text-test.rkt
(require rackunit "../../kernel/text.rkt"
         "../../kernel/gap/marker.rkt")

(test-case "construction"
  (define tx (make-text "hello"))
  (check-equal? (text-length tx) 5)
  (check-equal? (text-byte-ref tx 0) (char->integer #\h)))

(test-case "insert + marker adjustment"
  (define tx (make-text "ab"))
  (define m (text-marker! tx 2 #t))
  (text-insert! tx 1 (string->bytes/utf-8 "XY"))
  (check-equal? (text-marker-pos tx m) 4))

(test-case "insert before marker"
  (define tx (make-text "abc"))
  (define m (text-marker! tx 2))
  (text-insert! tx 0 (string->bytes/utf-8 "X"))
  (check-equal? (text-marker-pos tx m) 3))

(test-case "insert after marker (no change)"
  (define tx (make-text "abc"))
  (define m (text-marker! tx 1))
  (text-insert! tx 2 (string->bytes/utf-8 "X"))
  (check-equal? (text-marker-pos tx m) 1))

(test-case "delete + marker adjustment"
  (define tx (make-text "abcdef"))
  (define m (text-marker! tx 2))
  (text-delete! tx 1 3)
  (check-equal? (text-marker-pos tx m) 1))

(test-case "delete range containing marker"
  (define tx (make-text "abcdef"))
  (define m (text-marker! tx 3))
  (text-delete! tx 2 5)
  (check-equal? (text-marker-pos tx m) 2))

(test-case "delete after marker (no change)"
  (define tx (make-text "abcdef"))
  (define m (text-marker! tx 1))
  (text-delete! tx 3 5)
  (check-equal? (text-marker-pos tx m) 1))

(test-case "marker kill"
  (define tx (make-text "abc"))
  (define m (text-marker! tx 1))
  (check-equal? (length (text-markers tx)) 1)
  (text-marker-kill! tx m)
  (check-equal? (length (text-markers tx)) 0))

(test-case "adjust-markers-insert! standalone"
  (define m1 (make-marker 0))
  (define m2 (make-marker 3 #t))
  (define m3 (make-marker 5))
  (define markers (list m1 m2 m3))
  (adjust-markers-insert! markers 3 2)
  (check-equal? (marker-pos m1) 0)
  (check-equal? (marker-pos m2) 5)
  (check-equal? (marker-pos m3) 7))

(test-case "adjust-markers-delete! standalone"
  (define m1 (make-marker 0))
  (define m2 (make-marker 3))
  (define m3 (make-marker 7))
  (define markers (list m1 m2 m3))
  (adjust-markers-delete! markers 2 6)
  (check-equal? (marker-pos m1) 0)
  (check-equal? (marker-pos m2) 2)
  (check-equal? (marker-pos m3) 3))
