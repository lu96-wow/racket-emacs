#lang racket

;; kernel/undo/recorder.rkt — Undo recorder: store + merge + commit
;;
;; Depends only on record.rkt for data types.
;; Does NOT execute undo/redo — that's the buffer protocol's job.
;; Does NOT know about text, markers, or point position.

(require "record.rkt")

(provide
 ;; struct
 undo-recorder? undo-recorder
 undo-recorder-undo-stack undo-recorder-redo-stack
 undo-recorder-pending
 set-undo-recorder-undo-stack! set-undo-recorder-redo-stack!
 set-undo-recorder-pending!

 ;; constructor
 make-undo-recorder

 ;; recording
 recorder-record-insert!   ; recorder byte-pos byte-pos -> void
 recorder-record-delete!   ; recorder string? byte-pos -> void

 ;; lifecycle
 recorder-commit!          ; recorder -> void
 recorder-push-boundary!)  ; recorder -> void (alias)

;; ============================================================
;; Struct
;; ============================================================

(struct undo-recorder
  ([undo-stack #:mutable]   ; (listof undo-group?) — newest first
   [redo-stack #:mutable]   ; (listof undo-group?) — newest first
   [pending #:mutable])     ; (listof (or/c undo-insert? undo-delete?)) — newest first
  #:transparent)

(define (make-undo-recorder)
  (undo-recorder '() '() '()))

;; ============================================================
;; Recording
;; ============================================================

(define (recorder-record-insert! rec byte-pos byte-end)
  ;; Record an insert. Merges with previous adjacent insert.
  ;; Text is NOT stored — it's already in the buffer for undo.
  (define p (undo-recorder-pending rec))
  (cond
    [(and (pair? p) (undo-insert? (car p))
          (= (undo-insert-end (car p)) byte-pos))
     ;; Extend previous insert — merge into one range
     (define prev (car p))
     (set-undo-recorder-pending! rec
       (cons (undo-insert (undo-insert-beg prev) byte-end)
             (cdr p)))]
    [else
     (set-undo-recorder-pending! rec
       (cons (undo-insert byte-pos byte-end) p))]))

(define (recorder-record-delete! rec text byte-pos)
  ;; Record a delete. Text IS stored so undo can restore it.
  (set-undo-recorder-pending! rec
    (cons (undo-delete text byte-pos)
          (undo-recorder-pending rec))))

;; ============================================================
;; Lifecycle
;; ============================================================

(define (recorder-commit! rec)
  ;; Commit pending records as a single undo group.
  ;; Pending records are stored newest-first; reverse to oldest-first.
  (when (pair? (undo-recorder-pending rec))
    (set-undo-recorder-undo-stack! rec
      (cons (undo-group (reverse (undo-recorder-pending rec)))
            (undo-recorder-undo-stack rec)))
    (set-undo-recorder-pending! rec '())
    ;; New edit invalidates redo history
    (set-undo-recorder-redo-stack! rec '())))

(define recorder-push-boundary! recorder-commit!)
