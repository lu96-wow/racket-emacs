#lang racket
;; test/kernel/kill-ring/kill-ring-test.rkt
(require rackunit "../../../kernel/kill-ring/kill-ring.rkt")

(test-case "kill-new then yank"
  ;; Clear state by pushing a known item
  (kill-new "alpha")
  (check-equal? (kill-ring-yank) "alpha"))

(test-case "kill-ring-pop walks back"
  (kill-new "a") (kill-new "b") (kill-new "c")
  (check-equal? (kill-ring-yank) "c")
  (check-equal? (kill-ring-pop) "b")
  (check-equal? (kill-ring-pop) "a"))

(test-case "kill-append concatenates"
  (kill-new "hello")
  (kill-append " world" #f)
  (check-equal? (kill-ring-yank) "hello world"))

(test-case "kill-append before prepends"
  (kill-new "world")
  (kill-append "hello " #t)
  (check-equal? (kill-ring-yank) "hello world"))

(test-case "kill-ring-empty? detects empty after pop"
  (kill-new "only")
  (check-false (kill-ring-empty?))
  (kill-ring-pop)
  ;; At this point the ring still has "only" (pop doesn't remove)
  (check-false (kill-ring-empty?)))

(test-case "current-kill returns most recent"
  (kill-new "latest")
  (check-equal? (current-kill) "latest"))
