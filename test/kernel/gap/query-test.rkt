#lang racket
;; test/kernel/gap/query-test.rkt
(require rackunit "../../../kernel/gap/gap.rkt"
         "../../../kernel/gap/query.rkt")

(test-case "gap-char ASCII"
  (define gb (make-gap-buffer "abc"))
  (check-equal? (gap-char gb 0) #\a)
  (check-equal? (gap-char gb 1) #\b)
  (check-equal? (gap-char gb 2) #\c))

(test-case "gap-char CJK"
  (define gb (make-gap-buffer "a你好b"))
  (check-equal? (gap-char gb 0) #\a)
  (check-equal? (gap-char gb 1) #\你)
  (check-equal? (gap-char gb 4) #\好)
  (check-equal? (gap-char gb 7) #\b))

(test-case "gap-char across gap"
  (define gb (make-gap-buffer "abcdef"))
  (gap-insert! gb 3 (string->bytes/utf-8 "XYZ"))
  (check-equal? (gap-char gb 0) #\a)
  (check-equal? (gap-char gb 3) #\X)
  (check-equal? (gap-char gb 6) #\d))

(test-case "navigation"
  (define gb (make-gap-buffer "a你好b"))
  (check-equal? (gap-next-char-pos gb 0) 1)
  (check-equal? (gap-next-char-pos gb 1) 4)
  (check-equal? (gap-prev-char-pos gb 4) 1)
  (check-equal? (gap-prev-char-pos gb 1) 0))

(test-case "navigation across gap"
  (define gb (make-gap-buffer "abcdef"))
  (gap-insert! gb 3 (string->bytes/utf-8 "XYZ"))
  (check-equal? (gap-next-char-pos gb 3) 4)
  (check-equal? (gap-next-char-pos gb 5) 6)
  (check-equal? (gap-prev-char-pos gb 6) 5))

(test-case "substring"
  (define gb (make-gap-buffer "hello世界"))
  (check-equal? (gap-substring gb 0 5) "hello")
  (check-equal? (gap-substring gb 5 (gap-length gb)) "世界")
  (check-equal? (gap-string gb) "hello世界"))

(test-case "scan byte"
  (define gb (make-gap-buffer "ab\ncd"))
  (define newline? (curry = (char->integer #\newline)))
  (check-equal? (gap-scan-byte gb 0 'forward newline?) 2)
  (check-equal? (gap-scan-byte gb 5 'backward newline?) 2))

(test-case "scan char"
  (define gb (make-gap-buffer "abcDEF"))
  (check-equal? (gap-scan-char gb 0 'forward char-upper-case?) 3)
  (check-equal? (gap-scan-char gb 6 'backward char-lower-case?) 2))

(test-case "match-str-at"
  (define gb (make-gap-buffer "#|block|#"))
  (check-true  (gap-match-str-at gb 0 "#|"))
  (check-true  (gap-match-str-at gb 7 "|#"))
  (check-false (gap-match-str-at gb 0 "|#")))

(test-case "at-bol?"
  (define gb (make-gap-buffer "a\nbc"))
  (check-true  (gap-at-bol? gb 0))
  (check-true  (gap-at-bol? gb 2))
  (check-false (gap-at-bol? gb 1))
  (check-false (gap-at-bol? gb 3)))

(test-case "read-delim-word"
  (define gb (make-gap-buffer "DELIM\nbody"))
  (define-values (word end) (gap-read-delim-word gb 0))
  (check-equal? word "DELIM")
  (check-equal? end 5))
