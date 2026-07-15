#lang racket

;; platform/file-io.rkt — Raw file I/O

(provide file->string string->file)

(define (file->string path)
  (bytes->string/utf-8 (file->bytes path)))

(define (string->file str path #:exists [exists 'replace])
  (display-to-file str path #:exists exists))
