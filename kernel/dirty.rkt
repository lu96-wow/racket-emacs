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

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)

  (test-case "dirty-insert! accumulates changes"
    (let ([db (make-dirty-buffer)])
      (define db1 (dirty-insert! db "hello" 0))
      (check-equal? (dirty-string db1) "hello")
      (check-true (dirty-dirty? db1))
      (check-equal? (dirty-extent db1) '(0 . 5))
      (check-equal? (length (dirty-buffer-changes db1)) 1)))

  (test-case "dirty-delete! accumulates changes"
    (let* ([db0 (make-dirty-buffer (make-buffer "test" "abcdef"))]
           [db1 (dirty-delete! db0 2 4)])
      (check-equal? (dirty-string db1) "abef")
      (check-true (dirty-dirty? db1))
      (check-equal? (dirty-extent db1) '(2 . 2))))

  (test-case "multiple mutations merge extent"
    (let* ([db0 (make-dirty-buffer (make-buffer "test" "abcdefgh"))]
           [db1 (dirty-insert! db0 "X" 2)]
           [db2 (dirty-delete! db1 5 7)])
      (check-equal? (dirty-string db2) "abXcdfgh")
      ;; Changes: insert at 2 (2,3), delete at 5 (5,5) → extent (2,5)
      (check-equal? (dirty-extent db2) '(2 . 5))))

  (test-case "no-op returns same db"
    (let ([db (make-dirty-buffer)])
      (define db1 (dirty-insert! db "" 0))
      (check-false (dirty-dirty? db1))))

  (test-case "dirty-clear! resets changes"
    (let* ([db0 (make-dirty-buffer)]
           [db1 (dirty-insert! db0 "hello" 0)]
           [db2 (dirty-clear! db1)])
      (check-equal? (dirty-string db2) "hello")
      (check-false (dirty-dirty? db2))))

  (test-case "point/mark delegated, no change tracking"
    (let ([db (make-dirty-buffer (make-buffer "test" "abcdef"))])
      (define db1 (dirty-set-point! db 3))
      (check-equal? (dirty-point db1) 3)
      (check-false (dirty-dirty? db1))))

  (test-case "dirty-undo! tracks change"
    (let* ([db0 (make-dirty-buffer (make-buffer "test" "abc"))]
           [db1 (dirty-insert! db0 "XYZ" 1)]
           [_   (dirty-commit! db1)]
           [db2 (dirty-undo! db1)])
      (check-equal? (dirty-string db2) "abc")
      (check-true (dirty-dirty? db2))
      (check-equal? (dirty-extent db2) '(1 . 4))))

  (test-case "event-loop simulation: insert → commit → clear → render"
    (let* ([db0 (make-dirty-buffer)]
           ;; Command executes
           [db1 (dirty-insert! db0 "hello" 0)]
           [db2 (dirty-set-point! db1 5)]
           [_   (dirty-commit! db2)]
           ;; Event loop checks
           [_   (check-true (dirty-dirty? db2))]
           [_   (check-equal? (dirty-extent db2) '(0 . 5))]
           ;; Render happens, then clear
           [db3 (dirty-clear! db2)]
           [_   (check-false (dirty-dirty? db3))]
           ;; Second command
           [db4 (dirty-insert! db3 " world" 5)]
           [db5 (dirty-set-point! db4 11)])
      (check-equal? (dirty-string db5) "hello world")
      (check-equal? (dirty-extent db5) '(5 . 11)))))
