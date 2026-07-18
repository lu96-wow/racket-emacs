#lang racket

;; kernel/undo/recorder.rkt — Undo Recorder: store + merge + commit
;;
;; ============================================================================
;; Manages three pieces of state:
;;   undo-stack  — committed undo groups (newest first)
;;   redo-stack  — redone groups (newest first), cleared on new edit
;;   pending     — uncommitted records (newest first), merged on insert
;;
;; Does NOT execute undo/redo — that's the buffer layer's responsibility.
;; Does NOT know about text, markers, or point position.
;; Depends only on record.rkt for data types.
;;
;; ============================================================================
;; Computation vs Application
;; ============================================================================
;;
;;   ── Pure Queries ──
;;     (none — this is all mutable state management)
;;
;;   ── Mutations ──
;;     recorder-record-insert!    record an insert
;;     recorder-record-delete!    record a delete
;;     recorder-commit!           commit pending to undo-stack
;;
;; ============================================================================
;; Insert Merging
;; ============================================================================
;;
;;   Consecutive insert records at adjacent positions are merged into
;;   one range.  This prevents undo from breaking a single typed word
;;   into individual character undos.
;;
;; ============================================================================

(require "record.rkt")

(provide
 ;; ── struct ──
 undo-recorder? undo-recorder
 undo-recorder-undo-stack undo-recorder-redo-stack
 undo-recorder-pending
 set-undo-recorder-undo-stack! set-undo-recorder-redo-stack!
 set-undo-recorder-pending!

 ;; ── constructor ──
 make-undo-recorder

 ;; ── recording ──
 recorder-record-insert!
 recorder-record-delete!

 ;; ── lifecycle ──
 recorder-commit!
 recorder-push-boundary!)  ;; alias for recorder-commit!

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
  ;; Record an insert at [byte-pos, byte-end).
  ;; Merges with previous adjacent insert (for character-by-character typing).
  ;; Text is NOT stored — it's already in the buffer for undo.
  (define p (undo-recorder-pending rec))
  (cond
    ;; Merge with previous adjacent insert
    [(and (pair? p)
          (undo-insert? (car p))
          (= (undo-insert-end (car p)) byte-pos))
     (define prev (car p))
     (set-undo-recorder-pending! rec
       (cons (undo-insert (undo-insert-beg prev) byte-end #f #f)
             (cdr p)))]
    ;; New insert record
    [else
     (set-undo-recorder-pending! rec
       (cons (undo-insert byte-pos byte-end #f #f) p))]))

(define (recorder-record-delete! rec text faces byte-pos)
  ;; Record a delete of `text` at `byte-pos`.
  ;; Text AND faces are stored so undo can restore both.
  (unless (string? text)
    (raise-argument-error 'recorder-record-delete! "string?" text))
  (unless (bytes? faces)
    (raise-argument-error 'recorder-record-delete! "bytes?" faces))
  (set-undo-recorder-pending! rec
    (cons (undo-delete text faces byte-pos)
          (undo-recorder-pending rec))))

;; ============================================================
;; Lifecycle
;; ============================================================

(define (recorder-commit! rec)
  ;; Commit pending records as a single undo group.
  ;; Pending records are stored newest-first during collection;
  ;; we reverse to chronological order (oldest-first) for the group.
  ;; Also clears redo stack — new edit invalidates redo history.
  (when (pair? (undo-recorder-pending rec))
    (set-undo-recorder-undo-stack! rec
      (cons (undo-group (reverse (undo-recorder-pending rec)))
            (undo-recorder-undo-stack rec)))
    (set-undo-recorder-pending! rec '())
    (set-undo-recorder-redo-stack! rec '())))

(define recorder-push-boundary! recorder-commit!)
