#lang racket

;; kernel/edit.rkt — Editing commands
;;
;; Composes dirty-buffer + kill-ring into editing operations.
;; Commands operate on dirty-buffer only; cmd-self-insert also takes a char.
;; Movement/mark commands return the same db with no new changes.
;;
;; Architecture:
;;   dirty-buffer  ← change-tracked buffer wrapper
;;   kill-ring     ← clipboard (module-level shared state)
;;   edit          ← commands on dirty-buffer

(require "dirty.rkt"
         "buffer.rkt"
         "data/text.rkt"
         "data/query.rkt"
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
 cmd-next-line cmd-prev-line

 ;; mark / region
 cmd-set-mark cmd-swap-point-and-mark
 )

;; ============================================================
;; Content-modifying commands
;; ============================================================

(define (cmd-self-insert db ch)
  (if ch
      (dirty-insert! db (string ch) (dirty-point db))
      db))

(define (cmd-newline db)
  (define pt (dirty-point db))
  (define db1 (dirty-insert! db "\n" pt))
  (dirty-set-point! db1 (add1 pt)))

(define (cmd-tab db)
  (dirty-insert! db "\t" (dirty-point db)))

(define (cmd-backward-delete db)
  (define pt (dirty-point db))
  (if (> pt 0)
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (text-gap (buffer-text buf))]
             [prev (gap-prev-char-pos gb pt)]
             [db1 (dirty-delete! db prev pt)])
        (dirty-set-point! db1 prev))
      db))

(define (cmd-forward-delete db)
  (define pt (dirty-point db))
  (define len (dirty-length db))
  (if (< pt len)
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (text-gap (buffer-text buf))]
             [next (gap-next-char-pos gb pt)]
             [db1 (dirty-delete! db pt next)])
        db1)
      db))

(define (cmd-kill-line db)
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

(define (cmd-yank db)
  (define text (kill-ring-yank))
  (if (and text (positive? (string-length text)))
      (let* ([pt  (dirty-point db)]
             [db1 (dirty-insert! db text pt)]
             [blen (bytes-length (string->bytes/utf-8 text))]
             [db2 (dirty-set-point! db1 (+ pt blen))])
        (set-box! yank-start-box pt)
        db2)
      db))

(define (cmd-yank-pop db)
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

(define (cmd-undo db)
  (dirty-commit! db)
  (dirty-undo! db))

(define (cmd-redo db)
  (dirty-redo! db))

;; ============================================================
;; Movement — point only, no content change
;; ============================================================

(define (cmd-forward-char db)
  (define pt (dirty-point db))
  (if (< pt (dirty-length db))
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (text-gap (buffer-text buf))]
             [next (gap-next-char-pos gb pt)])
        (dirty-set-point! db next))
      db))

(define (cmd-backward-char db)
  (define pt (dirty-point db))
  (if (> pt 0)
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (text-gap (buffer-text buf))]
             [prev (gap-prev-char-pos gb pt)])
        (dirty-set-point! db prev))
      db))

(define (cmd-beginning-of-line db)
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

(define (cmd-end-of-line db)
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

(define (cmd-next-line db)
  (define buf (dirty-buffer-buf db))
  (define gb  (text-gap (buffer-text buf)))
  (define pt  (dirty-point db))
  (define len (dirty-length db))
  (define nl  (gap-scan-byte gb pt 'forward (lambda (b) (= b #x0A))))
  (dirty-set-point! db (if (>= nl len) len (add1 nl))))

(define (cmd-prev-line db)
  (define buf (dirty-buffer-buf db))
  (define gb  (text-gap (buffer-text buf)))
  (define pt  (dirty-point db))
  (if (zero? pt)
      db
      (let ([nl (gap-scan-byte gb (sub1 pt)
                    'backward (lambda (b) (= b #x0A)))])
        (if (< nl 0)
            (dirty-set-point! db 0)
            (dirty-set-point! db (add1 nl))))))

;; ============================================================
;; Mark / Region
;; ============================================================

(define (cmd-set-mark db)
  (dirty-set-mark! db))

(define (cmd-swap-point-and-mark db)
  (define buf (dirty-buffer-buf db))
  (if (region-active? buf)
      (let* ([m (buffer-mark buf)]
             [mp (text-marker-pos (buffer-text buf) m)]
             [pt (dirty-point db)]
             [db1 (dirty-set-point! db mp)])
        (text-set-marker-pos! (buffer-text buf) m pt)
        db1)
      db))
