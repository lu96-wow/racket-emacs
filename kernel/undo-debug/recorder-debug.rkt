#lang racket

;; kernel/undo-debug/recorder-debug.rkt — Recorder debug S-expression

(require "../undo/recorder.rkt")

(provide recorder-debug-summary)  ;; → "(undo (stack N) (redo M) (pending P))"

(define (recorder-debug-summary rec)
  (format "(undo (stack ~a) (redo ~a) (pending ~a))"
          (length (undo-recorder-undo-stack rec))
          (length (undo-recorder-redo-stack rec))
          (length (undo-recorder-pending rec))))
