#lang racket
;; test/kernel/gap/gap-test.rkt
(require rackunit "../../../kernel/gap/gap.rkt")

(test-case "construction"
  (define gb (make-gap-buffer "hello"))
  (check-equal? (gap-length gb) 5)
  (check-equal? (gap-byte-ref gb 0) (char->integer #\h))
  (check-equal? (gap-byte-ref gb 4) (char->integer #\o)))

(test-case "insert"
  (define gb (make-gap-buffer "ab"))
  (gap-insert! gb 1 (string->bytes/utf-8 "xy"))
  (check-equal? (gap-subbytes gb 0 (gap-length gb))
                (string->bytes/utf-8 "axyb")))

(test-case "delete"
  (define gb (make-gap-buffer "abcdef"))
  (gap-delete! gb 1 3)
  (check-equal? (gap-length gb) 4)
  (check-equal? (gap-subbytes gb 0 (gap-length gb))
                (string->bytes/utf-8 "adef")))

(test-case "insert UTF-8"
  (define gb (make-gap-buffer "ab"))
  (gap-insert! gb 2 (string->bytes/utf-8 "你好"))
  (check-equal? (gap-length gb) 8)
  (check-equal? (gap-subbytes gb 0 (gap-length gb))
                (string->bytes/utf-8 "ab你好")))

(test-case "delete spans gap"
  (define gb (make-gap-buffer "abcdef"))
  (gap-insert! gb 3 (string->bytes/utf-8 "XYZ"))
  (gap-delete! gb 1 4)
  (check-equal? (gap-subbytes gb 0 (gap-length gb))
                (string->bytes/utf-8 "aYZdef")))

(test-case "subbytes across gap"
  (define gb (make-gap-buffer "abcdef"))
  (gap-insert! gb 3 (string->bytes/utf-8 "XYZ"))
  (check-equal? (gap-subbytes gb 0 (gap-length gb))
                (bytes-append (string->bytes/utf-8 "abcXYZ")
                              (string->bytes/utf-8 "def"))))
