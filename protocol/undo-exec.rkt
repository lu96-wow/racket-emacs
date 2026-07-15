#lang racket

;; protocol/undo-exec.rkt — Undo/redo execution engine
;;
;; Pure functions that apply or reverse undo records on a text.
;; Composes: kernel/text.rkt + kernel/undo/record.rkt
;;
;; Key design: undo-insert does NOT store text (text is already in
;; the buffer when undo happens).  On redo, the range is empty —
;; execute-redo! treats it as a no-op.

(require "../kernel/text.rkt"
         "../kernel/undo/record.rkt")

(provide
 execute-undo!   ; text undo-group -> void
 execute-redo!)  ; text undo-group -> void

;; ============================================================
;; execute-undo!
;; ============================================================
;; Walk records in REVERSE (apply inverse of each operation).

(define (execute-undo! tx group)
  (define records (undo-group-records group))
  (for ([rec (in-list (reverse records))])
    (cond
      [(undo-insert? rec)
       ;; Undo an insert → delete the inserted range.
       ;; Text is still in the buffer at this point.
       (text-delete! tx (undo-insert-beg rec) (undo-insert-end rec))]
      [(undo-delete? rec)
       ;; Undo a delete → re-insert the stored text.
       (text-insert! tx
                     (undo-delete-beg rec)
                     (string->bytes/utf-8 (undo-delete-text rec)))])))

;; ============================================================
;; execute-redo!
;; ============================================================
;; Walk records FORWARD.
;; For undo-insert: undo already deleted the text, so redo is a no-op
;;   (delete on an empty range does nothing).
;; For undo-delete: undo re-inserted the text, so redo deletes it.

(define (execute-redo! tx group)
  (define records (undo-group-records group))
  (for ([rec (in-list records)])
    (cond
      [(undo-insert? rec)
       ;; Redo of an insert: text was already deleted by undo.
       ;; Delete again — range is empty, this is effectively a no-op.
       (text-delete! tx (undo-insert-beg rec) (undo-insert-end rec))]
      [(undo-delete? rec)
       ;; Redo of a delete: text was restored by undo, delete it again.
       (define dtext (undo-delete-text rec))
       (define dlen (bytes-length (string->bytes/utf-8 dtext)))
       (text-delete! tx (undo-delete-beg rec) (+ (undo-delete-beg rec) dlen))])))
