#lang racket

;; kernel/edit.rkt — Editing commands
;;
;; Composes dirty-buffer + key-event + kill-ring into editing operations.
;; Every command: dirty-buffer × key-event → dirty-buffer.
;;
;; Content-modifying commands return a dirty-buffer with accumulated
;; changes.  Movement/mark commands return the same db with no new changes
;; (point/mark movement is not a content change for display purposes).
;;
;; Architecture:
;;   dirty-buffer  ← change-tracked buffer wrapper
;;   key-event     ← input event type
;;   kill-ring     ← clipboard (module-level shared state)
;;   edit          ← commands = dirty-buffer × event → dirty-buffer

(require "dirty.rkt"
         "buffer.rkt"
         "data/text.rkt"
         "data/query.rkt"
         "key-event.rkt"
         "kill-ring.rkt"
         "undo/recorder.rkt")

(provide
 ;; content-modifying
 cmd-self-insert cmd-newline cmd-tab
 cmd-backward-delete cmd-forward-delete
 cmd-kill-line

 ;; yank / paste
 cmd-yank cmd-yank-pop

 ;; undo / redo
 cmd-undo cmd-redo

 ;; movement (point only)
 cmd-forward-char cmd-backward-char
 cmd-beginning-of-line cmd-end-of-line

 ;; mark / region
 cmd-set-mark cmd-swap-point-and-mark
 )

;; ============================================================
;; Content-modifying commands
;; ============================================================

(define (cmd-self-insert db evt)
  (define ch (key-event-char evt))
  (if ch
      (dirty-insert! db (string ch) (dirty-point db))
      db))

(define (cmd-newline db evt)
  (define pt (dirty-point db))
  (define db1 (dirty-insert! db "\n" pt))
  (dirty-set-point! db1 (add1 pt)))

(define (cmd-tab db evt)
  (dirty-insert! db "\t" (dirty-point db)))

(define (cmd-backward-delete db evt)
  (define pt (dirty-point db))
  (if (> pt 0)
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (text-gap (buffer-text buf))]
             [prev (gap-prev-char-pos gb pt)]
             [db1 (dirty-delete! db prev pt)])
        (dirty-set-point! db1 prev))
      db))

(define (cmd-forward-delete db evt)
  (define pt (dirty-point db))
  (define len (dirty-length db))
  (if (< pt len)
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (text-gap (buffer-text buf))]
             [next (gap-next-char-pos gb pt)]
             [db1 (dirty-delete! db pt next)])
        db1)
      db))

(define (cmd-kill-line db evt)
  (define buf (dirty-buffer-buf db))
  (define gb  (text-gap (buffer-text buf)))
  (define pt  (dirty-point db))
  (define len (dirty-length db))
  (define eol
    (let loop ([p pt])
      (cond [(>= p len) len]
            [(char=? (gap-char gb p) #\newline) p]
            [else (loop (gap-next-char-pos gb p))])))
  (cond
    [(= pt eol)
     ;; At end of line: kill the newline
     (if (< pt len)
         (let* ([next (gap-next-char-pos gb pt)]
                [db1  (dirty-delete! db pt next)])
           (kill-new "\n")
           (yank-mark-start! db1 pt))
         db)]
    [else
     (let* ([text (dirty-substring db pt eol)]
            [db1  (dirty-delete! db pt eol)])
       (kill-new text)
       (yank-mark-start! db1 pt))]))

;; ============================================================
;; Yank / Paste
;; ============================================================

(define yank-start-box (box #f))

(define (yank-mark-start! db pos)
  (set-box! yank-start-box pos)
  db)

(define (cmd-yank db evt)
  (define text (kill-ring-yank))
  (if (and text (positive? (string-length text)))
      (let* ([pt  (dirty-point db)]
             [db1 (dirty-insert! db text pt)]
             [blen (bytes-length (string->bytes/utf-8 text))]
             [db2 (dirty-set-point! db1 (+ pt blen))])
        (set-box! yank-start-box pt)
        db2)
      db))

(define (cmd-yank-pop db evt)
  (define prev-start (unbox yank-start-box))
  (define text (or (kill-ring-pop) (current-kill) ""))
  (if (and prev-start (positive? (string-length text)))
      (let* ([blen-prev (bytes-length (string->bytes/utf-8
                                       (or (kill-ring-yank) "")))]
             [db1 (if (> blen-prev 0)
                      (dirty-delete! db prev-start (+ prev-start blen-prev))
                      db)]
             [db2 (dirty-insert! db1 text prev-start)]
             [blen-new (bytes-length (string->bytes/utf-8 text))]
             [db3 (dirty-set-point! db2 (+ prev-start blen-new))])
        db3)
      db))

;; ============================================================
;; Undo / Redo
;; ============================================================

(define (cmd-undo db evt)
  (dirty-commit! db)
  (dirty-undo! db))

(define (cmd-redo db evt)
  (dirty-redo! db))

;; ============================================================
;; Movement — point only, no content change
;; ============================================================

(define (cmd-forward-char db evt)
  (define pt (dirty-point db))
  (if (< pt (dirty-length db))
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (text-gap (buffer-text buf))]
             [next (gap-next-char-pos gb pt)])
        (dirty-set-point! db next))
      db))

(define (cmd-backward-char db evt)
  (define pt (dirty-point db))
  (if (> pt 0)
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (text-gap (buffer-text buf))]
             [prev (gap-prev-char-pos gb pt)])
        (dirty-set-point! db prev))
      db))

(define (cmd-beginning-of-line db evt)
  (define pt (dirty-point db))
  (if (zero? pt)
      db
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (text-gap (buffer-text buf))]
             [bol (let loop ([p (gap-prev-char-pos gb pt)])
                    (if (zero? p)
                        0
                        (if (char=? (gap-char gb p) #\newline)
                            (gap-next-char-pos gb p)
                            (loop (gap-prev-char-pos gb p)))))])
        (dirty-set-point! db bol))))

(define (cmd-end-of-line db evt)
  (define pt (dirty-point db))
  (define len (dirty-length db))
  (if (>= pt len)
      db
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (text-gap (buffer-text buf))]
             [eol (let loop ([p pt])
                    (cond [(>= p len) len]
                          [(char=? (gap-char gb p) #\newline) p]
                          [else (loop (gap-next-char-pos gb p))]))])
        (dirty-set-point! db eol))))

;; ============================================================
;; Mark / Region
;; ============================================================

(define (cmd-set-mark db evt)
  (dirty-set-mark! db))

(define (cmd-swap-point-and-mark db evt)
  (define buf (dirty-buffer-buf db))
  (if (region-active? buf)
      (let* ([m (buffer-mark buf)]
             [mp (text-marker-pos (buffer-text buf) m)]
             [pt (dirty-point db)]
             [db1 (dirty-set-point! db mp)])
        (text-set-marker-pos! (buffer-text buf) m pt)
        db1)
      db))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)

  (define (make-ev ch)
    (key-event ch #f #f #f #f))

  (test-case "self-insert"
    (let* ([db  (make-dirty-buffer)]
           [db1 (cmd-self-insert db (make-ev #\a))]
           [db2 (cmd-self-insert db1 (make-ev #\b))])
      (check-equal? (dirty-string db2) "ab")
      (check-equal? (dirty-point db2) 2)
      (check-true (dirty-dirty? db2))))

  (test-case "newline"
    (let* ([db  (make-dirty-buffer)]
           [db1 (cmd-self-insert db (make-ev #\a))]
           [db2 (cmd-newline db1 (make-ev #\newline))]
           [db3 (cmd-self-insert db2 (make-ev #\b))])
      (check-equal? (dirty-string db3) "a\nb")
      (check-equal? (dirty-point db3) 3)))

  (test-case "backward-delete"
    (let* ([db0 (make-dirty-buffer (make-buffer "test" "abc"))]
           [db1 (dirty-set-point! db0 3)]
           [db2 (cmd-backward-delete db1 (key-event #f #f #f #f 'backspace))])
      (check-equal? (dirty-string db2) "ab")
      (check-equal? (dirty-point db2) 2)))

  (test-case "forward-delete"
    (let* ([db0 (make-dirty-buffer (make-buffer "test" "abc"))]
           [db1 (dirty-set-point! db0 1)]
           [db2 (cmd-forward-delete db1 (key-event #f #f #f #f 'delete))])
      (check-equal? (dirty-string db2) "ac")
      (check-equal? (dirty-point db2) 1)))

  (test-case "kill-line"
    (let* ([db0 (make-dirty-buffer (make-buffer "test" "line1\nline2\n"))]
           [db1 (cmd-kill-line db0 (key-event #f #t #f #f #f))]
           [db2 (dirty-commit! db1)])
      (check-equal? (dirty-string db2) "\nline2\n")
      (check-equal? (kill-ring-yank) "line1")))

  (test-case "yank"
    (let* ([db0 (make-dirty-buffer (make-buffer "test" "abc"))]
           [_   (kill-new "XYZ")]
           [db1 (dirty-set-point! db0 1)]
           [db2 (cmd-yank db1 (key-event #f #f #t #f #f))])
      (check-equal? (dirty-string db2) "aXYZbc")
      (check-equal? (dirty-point db2) 4)))

  (test-case "undo"
    (let* ([db0 (make-dirty-buffer (make-buffer "test" "abc"))]
           [db1 (cmd-self-insert db0 (make-ev #\X))]
           [db2 (dirty-commit! db1)]
           [db3 (cmd-undo db2 (key-event #f #t #f #f #f))])
      (check-equal? (dirty-string db3) "abc")))

  (test-case "movement does not mark dirty"
    (let* ([db0 (make-dirty-buffer (make-buffer "test" "abcdef"))]
           [db1 (dirty-clear! db0)]
           [db2 (dirty-set-point! db1 2)]
           [db3 (cmd-forward-char db2 (make-ev #\f))])
      (check-equal? (dirty-point db3) 3)
      (check-false (dirty-dirty? db3))))

  (test-case "beginning-of-line"
    (let* ([db0 (make-dirty-buffer (make-buffer "test" "hello\nworld"))]
           [db1 (dirty-set-point! db0 9)]   ; at 'l' in "world"
           [db2 (cmd-beginning-of-line db1 (make-ev #\a))])
      (check-equal? (dirty-point db2) 6)))  ; beginning of "world"

  (test-case "end-of-line"
    (let* ([db0 (make-dirty-buffer (make-buffer "test" "hello\nworld"))]
           [db1 (dirty-set-point! db0 7)]
           [db2 (cmd-end-of-line db1 (make-ev #\e))])
      (check-equal? (dirty-point db2) 11)))

  (test-case "set-mark and swap"
    (let* ([db0 (make-dirty-buffer (make-buffer "test" "hello world"))]
           [db1 (dirty-set-point! db0 3)]
           [db2 (cmd-set-mark db1 (make-ev #\space))]
           [db3 (dirty-set-point! db2 8)]
           [_   (check-true (dirty-region-active? db3))]
           [db4 (cmd-swap-point-and-mark db3 (make-ev #\x))])
      (check-equal? (dirty-point db4) 3)
      (check-true (dirty-region-active? db4))))

  (test-case "command composition: insert → delete → point → dirty extent"
    (let* ([db0 (make-dirty-buffer)]
           [db1 (cmd-self-insert db0 (make-ev #\H))]
           [db2 (cmd-self-insert db1 (make-ev #\e))]
           [db3 (cmd-self-insert db2 (make-ev #\y))]
           ;; Accumulate all changes
           [_   (check-equal? (dirty-extent db3) '(0 . 3))]
           ;; Clear (simulating render)
           [db4 (dirty-clear! db3)]
           [db5 (dirty-set-point! db4 0)]
           [db6 (cmd-forward-delete db5 (key-event #f #f #f #f 'delete))]
           [db7 (cmd-forward-delete db6 (key-event #f #f #f #f 'delete))])
      (check-equal? (dirty-string db7) "y")
      (check-equal? (dirty-extent db7) '(0 . 0))))
)
