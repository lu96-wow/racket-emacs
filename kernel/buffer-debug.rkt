#lang racket

;; kernel/buffer-debug.rkt — Buffer state debug S-expression

(require "buffer.rkt"
         "data/text.rkt"
         "data/face.rkt"
         "data-debug/gap-debug.rkt"
         "data-debug/face-debug.rkt"
         "undo-debug/recorder-debug.rkt")

(provide buffer-debug-summary)  ;; → "(buffer NAME (len ...) (pt ...) ... )"

(define (buffer-debug-summary buf)
  (define gb (text-gap (buffer-text buf)))
  (format "(buffer ~s (len ~a) (pt ~a) (mod ~a) (ro ~a) (modiff ~a) ~a ~a ~a)"
          (buffer-name buf)
          (buffer-length buf)
          (buffer-point buf)
          (buffer-modified? buf)
          (buffer-read-only? buf)
          (buffer-modiff buf)
          (gap-debug-summary gb)
          (face-debug-summary gb)
          (recorder-debug-summary (buffer-undo-recorder buf))))
