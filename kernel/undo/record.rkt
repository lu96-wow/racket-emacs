#lang racket

;; kernel/undo/record.rkt — Undo record types (pure data)
;;
;; No logic, no mutation, no dependencies.
;;
;; undo-insert  : text was inserted at [beg, end). Text is NOT stored
;;                because it's already in the buffer when undo happens.
;;                On redo, the text is gone — redo is a no-op (move point).
;; undo-delete  : text was deleted at byte position beg. Text IS stored
;;                so undo can restore it.
;; undo-group   : one command's worth of records, oldest first.
;;
;; pt-at-end? is deliberately absent — point restoration is
;; the buffer protocol layer's responsibility.

(provide
 undo-insert undo-insert? undo-insert-beg undo-insert-end
 undo-insert-text set-undo-insert-text!
 undo-delete undo-delete? undo-delete-text undo-delete-beg
 undo-group undo-group? undo-group-records)

(struct undo-insert (beg end [text #:mutable]) #:transparent)
;; text is #f until undo captures it; then redo uses it to re-insert

(struct undo-delete (text beg) #:transparent)

(struct undo-group (records) #:transparent)
