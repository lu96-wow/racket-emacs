#lang racket

;; user/file-io.rkt — File open/save (high-level, uses platform + kernel + mode)

(require "../kernel/buffer.rkt"
         "../base/registry.rkt"
         "../platform/file-io.rkt"
         "mode.rkt")

(provide find-file save-buffer buffer-file-name)

(define (find-file path)
  (define abs-path (path->complete-path (simplify-path path)))
  (define existing
    (for/or ([b (in-hash-values (buffer-registry-by-name global-registry))])
      (and (buffer-filename b) (equal? (buffer-filename b) (path->string abs-path)) b)))
  (if existing
      (begin (set-buffer existing) existing)
      (let* ([name (file-name-from-path abs-path)]
             [buf (get-buffer-create name)])
        (set-buffer-filename! buf (path->string abs-path))
        (define text (file->string abs-path))
        (buffer-insert buf text #:at 0)
        (set-buffer-modified?! buf #f)
        (set-buffer-saved-modiff! buf (buffer-modiff buf))
        (set-buffer-point! buf 0)
        (set-buffer buf)
        (auto-setup-buffer! buf abs-path)
        buf)))

(define (save-buffer)
  (define buf (current-buffer))
  (define fn (buffer-filename buf))
  (unless fn (error 'save-buffer "no file name"))
  (string->file (buffer-string buf) fn #:exists 'replace)
  (set-buffer-modified?! buf #f)
  (set-buffer-saved-modiff! buf (buffer-modiff buf))
  #t)

(define (buffer-file-name [buf (current-buffer)])
  (buffer-filename buf))

(define (file-name-from-path p)
  (define parts (explode-path p))
  (if (null? parts) "untitled" (path->string (last parts))))
