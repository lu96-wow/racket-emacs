#lang racket
;; test/kernel/key-event/key-event-test.rkt
(require rackunit "../../../kernel/key-event/key-event.rkt")

(test-case "self-insert classification"
  (check-true  (self-insert-key? (key-event #\a #f #f #f #f)))
  (check-true  (self-insert-key? (key-event #\space #f #f #f #f)))
  (check-true  (self-insert-key? (key-event #\1 #f #f #f #f)))
  (check-false (self-insert-key? (key-event #\a #t #f #f #f)))
  (check-false (self-insert-key? (key-event #\a #f #t #f #f)))
  (check-false (self-insert-key? (key-event #f #f #f #f 'up))))

(test-case "backspace classification"
  (check-true  (backspace-key? (key-event #f #f #f #f 'backspace)))
  (check-false (backspace-key? (key-event #\a #f #f #f #f)))
  (check-false (backspace-key? (key-event #f #f #f #f 'delete))))

(test-case "return classification"
  (check-true  (return-key? (key-event #f #f #f #f 'return)))
  (check-false (return-key? (key-event #\newline #f #f #f #f)))
  (check-false (return-key? (key-event #f #f #f #f 'tab))))

(test-case "cancel classification"
  (check-true  (cancel-key? (key-event #f #f #f #f 'escape)))
  (check-true  (cancel-key? (key-event #f #f #f #f 'cancel)))
  (check-false (cancel-key? (key-event #\g #t #f #f #f)))
  (check-false (cancel-key? (key-event #\a #f #f #f #f))))

(test-case "key-symbol?"
  (check-true  (key-symbol? 'up))
  (check-true  (key-symbol? 'f5))
  (check-true  (key-symbol? 'escape))
  (check-true  (key-symbol? 'cancel))
  (check-true  (key-symbol? 'backspace))
  (check-false (key-symbol? 'foo))
  (check-false (key-symbol? 'x)))

(test-case "description"
  (check-equal? (key-event->description (key-event #\a #f #f #f #f)) "a")
  (check-equal? (key-event->description (key-event #\a #t #f #f #f)) "C-a")
  (check-equal? (key-event->description (key-event #\space #f #t #f #f)) "M-SPC")
  (check-equal? (key-event->description (key-event #f #f #f #f 'up)) "up")
  (check-equal? (key-event->description (key-event #f #f #f #f 'escape)) "escape")
  (check-equal? (key-event->description (key-event #f #t #t #t 'cancel)) "C-M-S-cancel"))
