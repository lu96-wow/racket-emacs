#lang racket

;; base/undo.rkt — Linear undo/redo with record merging
;;
;; Each edit command produces a group of records. Consecutive inserts
;; at the same position are merged. Dependency-free.

(provide
 ;; ── record types ──
 undo-insert? undo-delete?
 undo-insert-beg undo-insert-end
 undo-delete-text undo-delete-beg undo-delete-pt-at-end?
 undo-group? undo-group-records undo-group

 ;; ── recorder ──
 make-undo-recorder undo-recorder?
 undo-recorder-undo-stack undo-recorder-redo-stack undo-recorder-pending
 set-undo-recorder-undo-stack! set-undo-recorder-redo-stack!
 set-undo-recorder-pending!
 undo-recorder-commit! undo-recorder-push-boundary!
 undo-recorder-record-insert! undo-recorder-record-delete!)

;; ============================================================
;; Record types
;; ============================================================

(struct undo-insert (beg end) #:transparent)
;; beg, end: byte positions — the inserted text occupied [beg, end)

(struct undo-delete (text beg pt-at-end?) #:transparent)
;; text: string — the deleted text (decoded)
;; beg: byte position where text was deleted
;; pt-at-end?: point was at end of deleted range → restore point there

;; A group = one command's worth of records, ordered oldest-first
(struct undo-group (records) #:transparent)

;; ============================================================
;; Undo Recorder — dual stack
;; ============================================================

(struct undo-recorder
  ([undo-stack #:mutable]   ; (listof undo-group?) — newest first
   [redo-stack #:mutable]   ; (listof undo-group?) — newest first
   [pending #:mutable])     ; (listof record) — current command's records
  #:transparent)

(define (make-undo-recorder)
  (undo-recorder '() '() '()))

(define (undo-recorder-push-boundary! rec)
  (undo-recorder-commit! rec))

(define (undo-recorder-commit! rec)
  (when (pair? (undo-recorder-pending rec))
    (set-undo-recorder-undo-stack! rec
      (cons (undo-group (reverse (undo-recorder-pending rec)))
            (undo-recorder-undo-stack rec)))
    (set-undo-recorder-pending! rec '())
    ;; New edit invalidates redo history
    (set-undo-recorder-redo-stack! rec '())))

(define (undo-recorder-record-insert! rec byte-pos byte-len)
  ;; Record an insert. Merges with previous insert if adjacent.
  (define p (undo-recorder-pending rec))
  (cond
    [(and (pair? p) (undo-insert? (car p))
          (= (undo-insert-end (car p)) byte-pos))
     ;; Extend previous insert
     (define prev (car p))
     (set-undo-recorder-pending! rec
       (cons (undo-insert (undo-insert-beg prev) (+ byte-pos byte-len))
             (cdr p)))]
    [else
     (set-undo-recorder-pending! rec
       (cons (undo-insert byte-pos (+ byte-pos byte-len)) p))]))

(define (undo-recorder-record-delete! rec byte-pos text pt-at-end?)
  (set-undo-recorder-pending! rec
    (cons (undo-delete text byte-pos pt-at-end?)
          (undo-recorder-pending rec))))
