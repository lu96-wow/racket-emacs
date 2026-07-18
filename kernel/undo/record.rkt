#lang racket

;; kernel/undo/record.rkt — Undo record types (pure data)
;;
;; ============================================================================
;; No logic, no mutation, no dependencies.  Just data types.
;; ============================================================================
;;
;;   undo-insert  : text + faces were inserted at [beg, end).
;;                  Text is NOT stored (already in buffer).
;;                  Faces captured before undo for redo restoration.
;;
;;   undo-delete  : text + faces were deleted at byte position `beg`.
;;                  Both text AND faces stored for undo restoration.
;;
;;   undo-group   : one command's worth of records, oldest-first.
;;
;; ============================================================================

(provide
 undo-insert undo-insert? undo-insert-beg undo-insert-end
 undo-insert-text set-undo-insert-text!
 undo-insert-faces set-undo-insert-faces!
 undo-delete undo-delete? undo-delete-text undo-delete-beg
 undo-delete-faces
 undo-group undo-group? undo-group-records)

;; ============================================================
;; undo-insert — text + faces inserted at [beg, end)
;; ============================================================

(struct undo-insert
  (beg                  ; exact-nonnegative-integer? — start of inserted range
   end                  ; exact-nonnegative-integer? — end of inserted range
   [text #:mutable]     ; (or/c string? #f) — captured text for redo
   [faces #:mutable])   ; (or/c bytes? #f) — captured face-ids for redo
  #:transparent)
;; `text` and `faces` are #f initially.  Before undo executes, buffer-undo!
;; captures both from the buffer so redo can restore them.

;; ============================================================
;; undo-delete — text + faces deleted at position `beg`
;; ============================================================

(struct undo-delete
  (text         ; string? — the deleted text (stored for undo restoration)
   faces        ; bytes? — the face-ids of the deleted range
   beg)          ; exact-nonnegative-integer? — position where text was deleted
  #:transparent)

;; ============================================================
;; undo-group
;; ============================================================

(struct undo-group
  (records)  ; (listof (or/c undo-insert? undo-delete?)) — oldest first
  #:transparent)
