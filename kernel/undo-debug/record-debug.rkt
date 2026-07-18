#lang racket

;; kernel/undo-debug/record-debug.rkt — Undo record debug S-expressions

(require "../undo/record.rkt")

(provide
 record-debug-summary  ;; → "(insert B E :text T :faces F)" | "(delete B TEXT LEN FLEN)"
 group-debug-summary)  ;; → "(group (insert ...) (delete ...) ...)"

(define (record-debug-summary rec)
  (cond
    [(undo-insert? rec)
     (format "(insert ~a ~a :text ~a :faces ~a)"
             (undo-insert-beg rec) (undo-insert-end rec)
             (if (undo-insert-text rec) 'yes 'no)
             (if (undo-insert-faces rec) 'yes 'no))]
    [(undo-delete? rec)
     (define txt (undo-delete-text rec))
     (define tlen (string-length txt))
     (define flen (bytes-length (undo-delete-faces rec)))
     (define preview
       (if (> tlen 20)
           (format "~s..." (substring txt 0 20))
           (format "~s" txt)))
     (format "(delete ~a ~a (len ~a) (faces ~a))"
             (undo-delete-beg rec) preview tlen flen)]
    [else (format "(unknown ~a)" rec)]))

(define (group-debug-summary group)
  (define recs (undo-group-records group))
  (format "(group ~a)"
          (string-join (map record-debug-summary recs) " ")))
