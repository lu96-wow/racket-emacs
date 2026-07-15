#lang racket
;; test/kernel/command/command-test.rkt
(require rackunit "../../../kernel/command/command.rkt")

(test-case "make-command-table is empty"
  (define t (make-command-table))
  (check-equal? (hash-count (command-table-map t)) 0))

(test-case "define-command! and lookup"
  (parameterize ([current-command-table (make-command-table)])
    (define-command! "hello" (λ () "world"))
    (define proc (lookup-command "hello"))
    (check-equal? (proc) "world")
    (check-false (lookup-command "nope"))))

(test-case "command-names"
  (parameterize ([current-command-table (make-command-table)])
    (define-command! "b" void)
    (define-command! "a" void)
    (define-command! "c" void)
    (check-equal? (command-names) '("a" "b" "c"))))

(test-case "compose two tables"
  (define t1 (make-command-table))
  (hash-set! (command-table-map t1) "a" "from-t1")
  (hash-set! (command-table-map t1) "b" "from-t1")
  (define t2 (make-command-table))
  (hash-set! (command-table-map t2) "b" "from-t2")
  (hash-set! (command-table-map t2) "c" "from-t2")
  (define composed (compose-command-tables t1 t2))
  ;; t2 overrides t1 for "b", adds "c"
  (check-equal? (hash-ref (command-table-map composed) "a") "from-t1")
  (check-equal? (hash-ref (command-table-map composed) "b") "from-t2")
  (check-equal? (hash-ref (command-table-map composed) "c") "from-t2")
  (check-equal? (hash-count (command-table-map composed)) 3))

(test-case "compose three tables"
  (define t1 (make-command-table))
  (hash-set! (command-table-map t1) "x" 1)
  (define t2 (make-command-table))
  (hash-set! (command-table-map t2) "y" 2)
  (define t3 (make-command-table))
  (hash-set! (command-table-map t3) "x" 3)
  (define composed (compose-command-tables t1 t2 t3))
  ;; t3 overrides t1 for "x"
  (check-equal? (hash-ref (command-table-map composed) "x") 3)
  (check-equal? (hash-ref (command-table-map composed) "y") 2))

(test-case "parameter isolation"
  (define t1 (make-command-table))
  (hash-set! (command-table-map t1) "a" "first")
  (parameterize ([current-command-table t1])
    (check-equal? (lookup-command "a") "first"))
  (define t2 (make-command-table))
  (hash-set! (command-table-map t2) "b" "second")
  (parameterize ([current-command-table t2])
    (check-equal? (lookup-command "a") #f)
    (check-equal? (lookup-command "b") "second")))
