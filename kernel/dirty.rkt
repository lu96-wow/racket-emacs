#lang racket

;; kernel/dirty.rkt — Dirty-buffer: buffer wrapper with edit position + delta
;;
;; ============================================================================
;; Wraps kernel/buffer.rkt.  Every content mutation returns a new
;; dirty-buffer with (edit-start . byte-delta) recorded.  The
;; incremental colorer uses edit-start to split the token tree and
;; delta to shift invalid tokens.
;;
;; ============================================================================
;; Computation vs Application
;; ============================================================================
;;
;;   ── Pure Queries (read-through to buffer) ──
;;     dirty-point dirty-region-active? dirty-region-beginning
;;     dirty-region-end dirty-length dirty-substring dirty-string
;;     dirty-change dirty-dirty?
;;
;;   ── Mutations (return new dirty-buffer with change marker) ──
;;     dirty-insert! dirty-delete! dirty-undo! dirty-redo!
;;     dirty-set-point! dirty-set-mark! dirty-deactivate-mark!
;;
;;   ── Change Tracking ──
;;     dirty-commit!  — commits undo recorder, returns SAME db
;;                      (side effect: buffer's undo-recorder is mutated,
;;                       but the dirty-buffer struct is unchanged.
;;                       The change marker stays — caller uses it for
;;                       coloring, then calls dirty-clear!)
;;     dirty-clear!   — clears the change marker, returns NEW db
;;
;; ============================================================================
;; Data Flow (main.rkt event loop)
;; ============================================================================
;;
;;  1. dispatch-key → new-db (with change marker)
;;  2. dirty-commit! → db (undo committed, change marker still set)
;;  3. colorer runs — reads dirty-change, colors the affected range
;;  4. dirty-clear! → db (change marker cleared)
;;  5. render — reads buffer content (doesn't care about change marker)
;;  6. next iteration — dirty-dirty? → #f (change was cleared)
;;
;; ============================================================================

(require "data/text.rkt"
         "buffer.rkt"
         "undo/recorder.rkt")

(provide
 ;; ── struct ──
 dirty-buffer? make-dirty-buffer
 dirty-buffer-buf dirty-buffer-change

 ;; ── content mutations ──
 dirty-insert! dirty-delete! dirty-undo! dirty-redo!

 ;; ── point / mark ──
 dirty-set-point! dirty-set-mark! dirty-deactivate-mark!

 ;; ── queries ──
 dirty-point dirty-region-active? dirty-region-beginning dirty-region-end
 dirty-length dirty-substring dirty-string

 ;; ── change tracking ──
 dirty-change       ;; → (or/c #f (cons/c pos delta))
 dirty-dirty?       ;; → boolean?
 dirty-commit!      ;; commit undo recorder, return same db
 dirty-clear!)      ;; clear change marker, return new db

;; ============================================================
;; Struct
;; ============================================================

(struct dirty-buffer
  (buf     ; buffer? — the underlying buffer (mutated in place)
   change) ; (or/c #f (cons/c exact-nonnegative-integer? exact-integer?))
           ;;   car = edit-start byte position
           ;;   cdr = net byte delta (> 0 insert, < 0 delete)
  #:transparent)

(define (make-dirty-buffer [buf (make-buffer)])
  (unless (buffer? buf)
    (raise-argument-error 'make-dirty-buffer "buffer?" buf))
  (dirty-buffer buf #f))

;; ============================================================
;; Content Mutations — return new dirty-buffer with change marker
;; ============================================================

(define (dirty-insert! db str byte-pos)
  (let-values ([(s e) (buffer-insert! (dirty-buffer-buf db) str byte-pos)])
    (if (< s e)
        (struct-copy dirty-buffer db
                     [change (cons s (- e s))])  ;; positive delta
        db)))

(define (dirty-delete! db from to)
  (let-values ([(s e) (buffer-delete! (dirty-buffer-buf db) from to)])
    (if (< from to)
        (struct-copy dirty-buffer db
                     [change (cons from (- from to))])  ;; negative delta
        db)))

(define (dirty-undo! db)
  (let-values ([(s e) (buffer-undo! (dirty-buffer-buf db))])
    (if s
        (struct-copy dirty-buffer db
                     [change (cons s (- e s))])
        db)))

(define (dirty-redo! db)
  (let-values ([(s e) (buffer-redo! (dirty-buffer-buf db))])
    (if s
        (struct-copy dirty-buffer db
                     [change (cons s (- e s))])
        db)))

;; ============================================================
;; Point / Mark — return same db (no content change)
;; ============================================================

(define (dirty-set-point! db pos)
  (set-buffer-point! (dirty-buffer-buf db) pos)
  db)

(define (dirty-set-mark! db)
  (set-mark! (dirty-buffer-buf db))
  db)

(define (dirty-deactivate-mark! db)
  (deactivate-mark! (dirty-buffer-buf db))
  db)

;; ============================================================
;; Queries — read-through to buffer
;; ============================================================

(define (dirty-point db)
  (buffer-point (dirty-buffer-buf db)))

(define (dirty-region-active? db)
  (region-active? (dirty-buffer-buf db)))

(define (dirty-region-beginning db)
  (region-beginning (dirty-buffer-buf db)))

(define (dirty-region-end db)
  (region-end (dirty-buffer-buf db)))

(define (dirty-length db)
  (buffer-length (dirty-buffer-buf db)))

(define (dirty-substring db from to)
  (buffer-substring (dirty-buffer-buf db) from to))

(define (dirty-string db)
  (buffer-string (dirty-buffer-buf db)))

;; ============================================================
;; Change Tracking
;; ============================================================

(define (dirty-change db)
  ;; Returns (cons edit-start delta) or #f if no change.
  ;; edit-start: byte position where the edit occurred.
  ;; delta: net byte change (> 0 insert, < 0 delete).
  ;; NOTE: Dirty-commit! does NOT clear the change marker.
  ;; The caller must call dirty-clear! after the colorer processes it.
  (dirty-buffer-change db))

(define (dirty-dirty? db)
  ;; Has there been a content change since the last dirty-clear!?
  (and (dirty-buffer-change db) #t))

(define (dirty-commit! db)
  ;; ── IMPORTANT: Side Effect ──
  ;; Commits pending undo records as a group in the buffer's undo-recorder.
  ;; The dirty-buffer struct ITSELF is unchanged — the change marker
  ;; (edit-start . delta) stays set.  The caller should:
  ;;   1. Read dirty-change for the colorer
  ;;   2. Then call dirty-clear! to reset the change marker
  ;; This separation exists because dirty-commit! only needs to mark
  ;; the undo boundary; it's dirty-clear! that signals "colorer is done".
  (recorder-commit! (buffer-undo-recorder (dirty-buffer-buf db)))
  db)

(define (dirty-clear! db)
  ;; Clear the change marker.  Returns a NEW dirty-buffer struct
  ;; with change = #f.  Call this AFTER the colorer has processed
  ;; the edit, so movement commands don't trigger stale re-coloring.
  (struct-copy dirty-buffer db [change #f]))
