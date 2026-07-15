#lang racket
;; test/kernel/undo/recorder-test.rkt
(require rackunit "../../../kernel/undo/recorder.rkt"
         "../../../kernel/undo/record.rkt")

(test-case "make-recorder starts empty"
  (define rec (make-undo-recorder))
  (check-equal? (undo-recorder-undo-stack rec) '())
  (check-equal? (undo-recorder-redo-stack rec) '())
  (check-equal? (undo-recorder-pending rec) '()))

(test-case "record and commit one insert"
  (define rec (make-undo-recorder))
  (recorder-record-insert! rec 0 5)
  (check-equal? (length (undo-recorder-pending rec)) 1)
  (recorder-commit! rec)
  (check-equal? (length (undo-recorder-undo-stack rec)) 1)
  (check-equal? (undo-recorder-pending rec) '())
  (define g (car (undo-recorder-undo-stack rec)))
  (check-equal? (length (undo-group-records g)) 1)
  (define r (car (undo-group-records g)))
  (check-true (undo-insert? r))
  (check-equal? (undo-insert-beg r) 0)
  (check-equal? (undo-insert-end r) 5))

(test-case "merge adjacent inserts"
  (define rec (make-undo-recorder))
  (recorder-record-insert! rec 0 3)
  (recorder-record-insert! rec 3 5)
  (recorder-commit! rec)
  (define g (car (undo-recorder-undo-stack rec)))
  (define r (car (undo-group-records g)))
  (check-equal? (length (undo-group-records g)) 1)
  (check-equal? (undo-insert-beg r) 0)
  (check-equal? (undo-insert-end r) 5))

(test-case "non-adjacent inserts do NOT merge"
  (define rec (make-undo-recorder))
  (recorder-record-insert! rec 0 2)
  (recorder-record-insert! rec 5 7)
  (recorder-commit! rec)
  (define g (car (undo-recorder-undo-stack rec)))
  (check-equal? (length (undo-group-records g)) 2))

(test-case "record and commit one delete"
  (define rec (make-undo-recorder))
  (recorder-record-delete! rec "xy" 2)
  (recorder-commit! rec)
  (define g (car (undo-recorder-undo-stack rec)))
  (define r (car (undo-group-records g)))
  (check-true (undo-delete? r))
  (check-equal? (undo-delete-text r) "xy")
  (check-equal? (undo-delete-beg r) 2))

(test-case "mixed inserts and deletes in one group"
  (define rec (make-undo-recorder))
  (recorder-record-delete! rec "old" 0)
  (recorder-record-insert! rec 0 3)
  (recorder-commit! rec)
  (define g (car (undo-recorder-undo-stack rec)))
  (check-equal? (length (undo-group-records g)) 2)
  (check-true (undo-delete? (first (undo-group-records g))))
  (check-true (undo-insert? (second (undo-group-records g)))))

(test-case "multiple commits stack correctly"
  (define rec (make-undo-recorder))
  (recorder-record-insert! rec 0 1)
  (recorder-commit! rec)
  (recorder-record-delete! rec "x" 5)
  (recorder-commit! rec)
  (check-equal? (length (undo-recorder-undo-stack rec)) 2))

(test-case "commit clears redo"
  (define rec (make-undo-recorder))
  (set-undo-recorder-redo-stack! rec (list (undo-group '())))
  (recorder-record-insert! rec 0 1)
  (recorder-commit! rec)
  (check-equal? (undo-recorder-redo-stack rec) '()))

(test-case "push-boundary is alias for commit"
  (define rec (make-undo-recorder))
  (recorder-record-insert! rec 0 1)
  (recorder-push-boundary! rec)
  (check-equal? (length (undo-recorder-undo-stack rec)) 1))
