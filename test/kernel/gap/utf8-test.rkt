#lang racket
;; test/kernel/gap/utf8-test.rkt
(require rackunit "../../../kernel/gap/utf8.rkt")

(test-case "classification"
  (check-true  (utf8-start-byte? (char->integer #\a)))
  (check-false (utf8-start-byte? #x80))
  (check-false (utf8-start-byte? #xBF))
  (check-equal? (utf8-char-len (char->integer #\a)) 1)
  (check-equal? (utf8-char-len #xC2) 2)
  (check-equal? (utf8-char-len #xE2) 3)
  (check-equal? (utf8-char-len #xF0) 4))

(test-case "encode ASCII roundtrip"
  (define bs (utf8-encode #\a))
  (check-equal? (bytes-length bs) 1)
  (check-equal? (bytes-ref bs 0) 97))

(test-case "encode CJK"
  (define bs (utf8-encode #\你))
  (check-equal? (bytes-length bs) 3))

(test-case "decode ASCII"
  (define bs (utf8-encode #\Z))
  (define-values (ch len) (utf8-decode bs 0))
  (check-equal? ch #\Z)
  (check-equal? len 1))

(test-case "decode CJK"
  (define bs (string->bytes/utf-8 "你好"))
  (define-values (ch1 len1) (utf8-decode bs 0))
  (check-equal? ch1 #\你)
  (check-equal? len1 3)
  (define-values (ch2 len2) (utf8-decode bs 3))
  (check-equal? ch2 #\好)
  (check-equal? len2 3))

(test-case "navigation"
  (define bs (string->bytes/utf-8 "a你好b"))
  (check-equal? (utf8-next-pos bs 0) 1)
  (check-equal? (utf8-next-pos bs 1) 4)
  (check-equal? (utf8-next-pos bs 4) 7)
  (check-equal? (utf8-prev-pos bs 4) 1)
  (check-equal? (utf8-prev-pos bs 1) 0)
  (check-equal? (utf8-prev-pos bs 0) 0))

(test-case "decode-reverse"
  (define bs (string->bytes/utf-8 "a你好"))
  (define-values (ch len) (utf8-decode-reverse bs (bytes-length bs)))
  (check-equal? ch #\好)
  (check-equal? len 3))
