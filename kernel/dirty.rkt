#lang racket

;; kernel/dirty.rkt — Dirty-buffer: a buffer wrapper that tracks change regions
;;
;; Wraps kernel/buffer.rkt with change-tracking.  Every mutation returns
;; a new dirty-buffer value accumulating the change extent.  The display
;; layer consumes dirty-extent and calls dirty-clear! after rendering.
;;
;; Zero global mutable state.  The event loop owns the dirty-buffer value
;; and threads it through each command invocation:
;;
;;   (event-loop db)  →  ((command-fn cmd) db evt)  →  new-db
;;
;; Architecture:
;;   buffer      — pure data model (text + undo + point/mark)
;;   dirty       — wraps buffer, adds change accumulation
;;   command     — takes dirty-buffer, returns dirty-buffer
;;   event-loop  — owns dirty-buffer, passes to commands, checks for render

(require "data/text.rkt"
         "buffer.rkt"
         "undo/recorder.rkt")

(provide
 ;; struct
 dirty-buffer? make-dirty-buffer
 dirty-buffer-buf dirty-buffer-changes

 ;; mutations — return new dirty-buffer
 dirty-insert! dirty-delete! dirty-undo! dirty-redo!

 ;; point / mark (delegated to buffer, return same db)
 dirty-set-point! dirty-set-mark! dirty-deactivate-mark!

 ;; queries (read-through)
 dirty-point dirty-region-active? dirty-region-beginning dirty-region-end
 dirty-length dirty-substring dirty-string

 ;; change tracking
 dirty-extent       ;; → (or/c #f (cons/c start end))
 dirty-clear!       ;; → new dirty-buffer with empty changes
 dirty-dirty?       ;; → boolean

 ;; buffer operations that also track
 dirty-commit!      ;; commit undo + return new db
 )

;; ============================================================
;; Struct
;; ============================================================

(struct dirty-buffer
  (buf      ; buffer? — the underlying buffer (mutated in place)
   changes) ; (listof (cons/c start end)) — change extents, newest first
  #:transparent)

(define (make-dirty-buffer [buf (make-buffer)])
  (dirty-buffer buf '()))

;; ============================================================
;; Mutations — all return new dirty-buffer with accumulated changes
;; ============================================================

(define (dirty-insert! db str byte-pos)
  (let-values ([(s e) (buffer-insert! (dirty-buffer-buf db) str byte-pos)])
    (if (< s e)
        (struct-copy dirty-buffer db
                     [changes (cons (cons s e) (dirty-buffer-changes db))])
        db)))

(define (dirty-delete! db from to)
  (buffer-delete! (dirty-buffer-buf db) from to)
  (if (< from to)
      (struct-copy dirty-buffer db
                   [changes (cons (cons from from) (dirty-buffer-changes db))])
      db))

(define (dirty-undo! db)
  (let-values ([(s e) (buffer-undo! (dirty-buffer-buf db))])
    (if s
        (struct-copy dirty-buffer db
                     [changes (cons (cons s e) (dirty-buffer-changes db))])
        db)))

(define (dirty-redo! db)
  (let-values ([(s e) (buffer-redo! (dirty-buffer-buf db))])
    (if s
        (struct-copy dirty-buffer db
                     [changes (cons (cons s e) (dirty-buffer-changes db))])
        db)))

(define (dirty-commit! db)
  ;; Commit undo recorder, track the command boundary.
  ;; Returns new db (changes list unchanged; caller typically calls this
  ;; between commands or after accumulating all changes for one command).
  (recorder-commit! (buffer-undo-recorder (dirty-buffer-buf db)))
  db)

;; ============================================================
;; Point / Mark — delegated to buffer, return same db (no content change)
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
;; Change tracking
;; ============================================================

(define (dirty-extent db)
  ;; Merge all accumulated changes into minimal byte range.
  ;; Returns (cons min-start max-end) or #f if no changes.
  (define cs (dirty-buffer-changes db))
  (and (pair? cs)
       (let loop ([cs cs] [mn +inf.0] [mx -inf.0])
         (if (null? cs)
             (cons (exact-floor mn) (exact-ceiling mx))
             (let ([c (car cs)])
               (loop (cdr cs) (min mn (car c)) (max mx (cdr c))))))))

(define (dirty-clear! db)
  ;; Return new dirty-buffer with empty changes list.
  (struct-copy dirty-buffer db [changes '()]))

(define (dirty-dirty? db)
  (pair? (dirty-buffer-changes db)))
