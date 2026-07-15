#lang racket
;; test/protocol/buffer-test.rkt
(require rackunit
         "../../protocol/buffer.rkt"
         "../../kernel/text.rkt"
         "../../kernel/undo/recorder.rkt")

(test-case "make-buffer"
  (define buf (make-buffer "test"))
  (check-equal? (buffer-name buf) "test")
  (check-equal? (buffer-length buf) 0)
  (check-false (buffer-modified? buf))
  (check-equal? (buffer-point buf) 0))

(test-case "make-buffer with initial text"
  (define buf (make-buffer "test" "hello"))
  (check-equal? (buffer-length buf) 5)
  (check-equal? (buffer-string buf) "hello"))

(test-case "buffer-insert!"
  (define buf (make-buffer "t"))
  (buffer-insert! buf "abc" 0)
  (check-equal? (buffer-string buf) "abc")
  (check-true (buffer-modified? buf)))

(test-case "buffer-insert! at point"
  (define buf (make-buffer "t" "ab"))
  (buffer-insert! buf "XY" (buffer-point buf))
  (check-equal? (buffer-string buf) "XYab"))

(test-case "buffer-insert! moves point"
  (define buf (make-buffer "t" "ab"))
  ;; point marker has insertion-type #t, so it stays after inserted text
  (set-buffer-point! buf 1)
  (buffer-insert! buf "XY" 1)
  (check-equal? (buffer-point buf) 3))  ;; point was at 1, insertion-type → moves past "XY"

(test-case "buffer-delete!"
  (define buf (make-buffer "t" "abcdef"))
  (buffer-delete! buf 1 4)
  (check-equal? (buffer-string buf) "aef")
  (check-equal? (buffer-length buf) 3))

(test-case "buffer-undo! (insert)"
  (define buf (make-buffer "t"))
  (buffer-insert! buf "hello" 0)
  (recorder-commit! (buffer-undo-recorder buf))
  (check-equal? (buffer-string buf) "hello")
  (check-true (buffer-undo! buf))
  (check-equal? (buffer-string buf) ""))

(test-case "buffer-undo! (delete)"
  (define buf (make-buffer "t" "abcdef"))
  (buffer-delete! buf 1 4)  ;; delete "bcd"
  (recorder-commit! (buffer-undo-recorder buf))
  (check-equal? (buffer-string buf) "aef")
  (check-true (buffer-undo! buf))  ;; should restore "bcd"
  (check-equal? (buffer-string buf) "abcdef"))

(test-case "buffer-redo! (delete)"
  (define buf (make-buffer "t" "abcdef"))
  (buffer-delete! buf 1 4)
  (recorder-commit! (buffer-undo-recorder buf))
  (buffer-undo! buf)
  (check-equal? (buffer-string buf) "abcdef")
  (check-true (buffer-redo! buf))
  (check-equal? (buffer-string buf) "aef"))

(test-case "undo then undo-insert-recorder-commit!"
  (define buf (make-buffer "t"))
  (buffer-insert! buf "first" 0)
  (recorder-commit! (buffer-undo-recorder buf))
  (buffer-insert! buf "second" 0)
  (recorder-commit! (buffer-undo-recorder buf))
  (check-equal? (buffer-string buf) "secondfirst")
  (buffer-undo! buf)
  (check-equal? (buffer-string buf) "first")
  (buffer-undo! buf)
  (check-equal? (buffer-string buf) ""))

(test-case "set-mark! and region"
  (define buf (make-buffer "t" "hello world"))
  (check-false (region-active? buf))
  (set-mark! buf)
  (check-false (region-active? buf))  ;; mark = point, not active
  (set-buffer-point! buf 5)
  (check-true (region-active? buf))
  (check-equal? (region-beginning buf) 0)
  (check-equal? (region-end buf) 5)
  (deactivate-mark! buf)
  (check-false (region-active? buf)))

(test-case "change-tracker"
  (define buf (make-buffer "t"))
  (check-false (buffer-change-region buf))
  (buffer-insert! buf "abc" 0)
  (define cr (buffer-change-region buf))
  (check-equal? (car cr) 0)
  (check-equal? (cdr cr) 3)
  (clear-buffer-change-region! buf)
  (check-false (buffer-change-region buf)))

(test-case "change-tracker extends range"
  (define buf (make-buffer "t"))
  (buffer-insert! buf "abc" 0)
  (buffer-insert! buf "XY" 5)
  (define cr (buffer-change-region buf))
  (check-equal? (car cr) 0)
  (check-equal? (cdr cr) 7))

(test-case "read-only buffer rejects edits"
  (define buf (make-buffer "t"))
  (set-buffer-read-only?! buf #t)
  (check-exn exn:fail? (λ () (buffer-insert! buf "x" 0)))
  (check-exn exn:fail? (λ () (buffer-delete! buf 0 1))))
