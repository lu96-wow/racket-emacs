#lang racket

;; kernel/undo/exec.rkt — Undo/Redo execution engine
;;
;; ============================================================================
;; Applies or reverses undo records on a text.  Calls text-insert!/text-delete!
;; directly (bypasses buffer.rkt) so no new undo records are created.
;;
;; Face preservation: exec.rkt composes text operations with face operations.
;; It accesses the gap buffer through text-gap to read/write face-ids.
;;
;; ============================================================================
;; Composition Pattern
;; ============================================================================
;;
;;   Undo a delete  → text-insert! bytes  +  face-fill! faces
;;   Redo an insert → text-insert! bytes  +  face-fill! faces
;;   Undo an insert → text-delete! bytes  (faces auto-deleted)
;;   Redo a delete  → text-delete! bytes  (faces auto-deleted)
;;
;; ============================================================================

(require "../data/text.rkt"
         "../data/face.rkt"
         "record.rkt")

(provide execute-undo! execute-redo!)

;; ============================================================
;; execute-undo!
;; ============================================================

(define (execute-undo! tx group)
  (unless (undo-group? group)
    (raise-argument-error 'execute-undo! "undo-group?" group))

  (define records (undo-group-records group))

  (for ([rec (in-list (reverse records))])
    (cond
      [(undo-insert? rec)
       ;; Undo an insert → delete the inserted range.
       (text-delete! tx (undo-insert-beg rec) (undo-insert-end rec))]

      [(undo-delete? rec)
       ;; Undo a delete → re-insert text, then restore faces.
       (define beg (undo-delete-beg rec))
       (define bs  (string->bytes/utf-8 (undo-delete-text rec)))
       (text-insert! tx beg bs)
       (face-copy! (text-gap tx) beg (undo-delete-faces rec))]

      [else
       (error 'execute-undo! "unknown undo record type: ~a" rec)])))

;; ============================================================
;; execute-redo!
;; ============================================================

(define (execute-redo! tx group)
  (unless (undo-group? group)
    (raise-argument-error 'execute-redo! "undo-group?" group))

  (define records (undo-group-records group))

  (for ([rec (in-list records)])
    (cond
      [(undo-insert? rec)
       (if (undo-insert-text rec)
           ;; Redo with captured text + faces: re-insert both.
           (let ([beg (undo-insert-beg rec)]
                 [bs  (string->bytes/utf-8 (undo-insert-text rec))])
             (text-insert! tx beg bs)
             (face-copy! (text-gap tx) beg (undo-insert-faces rec)))
           ;; Redo without text: undo of an insert → delete the range.
           (text-delete! tx (undo-insert-beg rec) (undo-insert-end rec)))]

      [(undo-delete? rec)
       ;; Redo a delete → delete the text that undo restored.
       (define dtext (undo-delete-text rec))
       (define dlen (bytes-length (string->bytes/utf-8 dtext)))
       (text-delete! tx
                     (undo-delete-beg rec)
                     (+ (undo-delete-beg rec) dlen))]

      [else
       (error 'execute-redo! "unknown undo record type: ~a" rec)])))
