#lang racket
;; test/kernel/undo/exec-test.rkt
(require rackunit
         "../../../kernel/undo/exec.rkt"
         "../../../kernel/text.rkt"
         "../../../kernel/undo/record.rkt")

(test-case "undo-insert: delete the inserted range"
  (define tx (make-text "hello"))
  (check-equal? (text-length tx) 5)
  (execute-undo! tx (undo-group (list (undo-insert 0 5))))
  (check-equal? (text-length tx) 0))

(test-case "undo-delete: re-insert deleted text"
  (define tx (make-text "abef"))
  (execute-undo! tx (undo-group (list (undo-delete "cd" 2))))
  (check-equal? (text-length tx) 6))

(test-case "redo-insert: no-op (text already deleted by undo)"
  (define tx (make-text))
  ;; After undo of insert, buffer is empty. Redo is a no-op.
  (execute-redo! tx (undo-group (list (undo-insert 0 5))))
  (check-equal? (text-length tx) 0))

(test-case "redo-delete: delete restored text"
  (define tx (make-text "abcdef"))
  (execute-redo! tx (undo-group (list (undo-delete "cd" 2))))
  (check-equal? (text-length tx) 4))

(test-case "undo then redo roundtrip (insert: redo is no-op)"
  (define tx (make-text "hello"))
  (define g (undo-group (list (undo-insert 0 5))))
  (execute-undo! tx g)
  (check-equal? (text-length tx) 0)
  (execute-redo! tx g)
  ;; Text was deleted by undo; redo is a no-op, text stays gone.
  (check-equal? (text-length tx) 0))

(test-case "undo then redo roundtrip (delete)"
  (define tx (make-text "abef"))
  (define g (undo-group (list (undo-delete "cd" 2))))
  (execute-undo! tx g)   ;; re-insert "cd"
  (check-equal? (text-length tx) 6)
  (execute-redo! tx g)   ;; delete "cd" again
  (check-equal? (text-length tx) 4))
