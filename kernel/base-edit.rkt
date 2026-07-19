#lang racket

;; kernel/base-edit.rkt — Basic editing commands (no syntax-table dependency)
;;
;; ============================================================================
;; Composes dirty-buffer + kill-ring into editing operations.
;; Commands operate on dirty-buffer only; cmd-self-insert also takes a char.
;; Movement/mark commands return the same db with no new changes.
;;
;; ============================================================================
;; Computation vs Application
;; ============================================================================
;;
;;   ── Content-Modifying (return new dirty-buffer with change marker) ──
;;     cmd-self-insert cmd-newline cmd-tab
;;     cmd-backward-delete cmd-forward-delete cmd-kill-line
;;     cmd-yank cmd-yank-pop
;;     cmd-undo cmd-redo
;;
;;   ── Movement (point only, no content change) ──
;;     cmd-forward-char cmd-backward-char
;;     cmd-beginning-of-line cmd-end-of-line
;;     cmd-next-line cmd-prev-line
;;
;;   ── Line Helpers (pure) ──
;;     line-beginning line-end display-column move-to-column
;;
;;   ── Mark / Region ──
;;     cmd-set-mark cmd-swap-point-and-mark
;;
;; ============================================================================

(require "dirty.rkt"
         "buffer.rkt"
         "data/text.rkt"
         "data/query.rkt"
         "kill-ring.rkt"
         "undo/recorder.rkt"
         "data/char-width.rkt")

(provide
 ;; ── content-modifying ──
 cmd-self-insert cmd-self-insert-string cmd-newline cmd-tab
 cmd-backward-delete cmd-forward-delete
 cmd-kill-line

 ;; ── region ──
 cmd-kill-region cmd-delete-region cmd-copy-region

 ;; ── yank / paste ──
 cmd-yank cmd-yank-pop

 ;; ── undo / redo ──
 cmd-undo cmd-redo

 ;; ── movement ──
 cmd-forward-char cmd-backward-char
 cmd-beginning-of-line cmd-end-of-line
 cmd-next-line cmd-prev-line

 ;; ── line helpers (pure) ──
 line-beginning line-end
 display-column move-to-column

 ;; ── mark / region ──
 cmd-set-mark cmd-swap-point-and-mark)

;; ============================================================
;; Helpers
;; ============================================================

(define (buf-gap db) (text-gap (buffer-text (dirty-buffer-buf db))))

;; ============================================================
;; Content-Modifying Commands
;; ============================================================

(define (cmd-self-insert db ch)
  (if ch
      (dirty-insert! db (string ch) (dirty-point db))
      db))

(define (cmd-self-insert-string db str)
  (if (and (string? str) (positive? (string-length str)))
      (let* ([pt (dirty-point db)]
             [db1 (dirty-insert! db str pt)]
             [blen (bytes-length (string->bytes/utf-8 str))])
        (dirty-set-point! db1 (+ pt blen)))
      db))

(define (cmd-newline db)
  (define pt (dirty-point db))
  (define db1 (dirty-insert! db "\n" pt))
  ;; Point moves AFTER the inserted newline (insertion-type = #t)
  (dirty-set-point! db1 (add1 pt)))

(define (cmd-tab db)
  (dirty-insert! db "\t" (dirty-point db)))

(define (cmd-backward-delete db)
  (define pt (dirty-point db))
  (if (> pt 0)
      (let* ([gb   (buf-gap db)]
             [prev (gap-prev-char-pos gb pt)]
             [db1  (dirty-delete! db prev pt)])
        (dirty-set-point! db1 prev))
      db))

(define (cmd-forward-delete db)
  (define pt (dirty-point db))
  (define len (dirty-length db))
  (if (< pt len)
      (let* ([gb   (buf-gap db)]
             [next (gap-next-char-pos gb pt)]
             [db1  (dirty-delete! db pt next)])
        db1)
      db))

(define (cmd-kill-line db)
  (define gb  (buf-gap db))
  (define pt  (dirty-point db))
  (define len (dirty-length db))
  (define eol
    (let loop ([p pt])
      (cond [(>= p len) len]
            [(char=? (gap-char gb p) #\newline) p]
            [else (loop (gap-next-char-pos gb p))])))
  (cond
    ;; Kill from point to end of line (excluding newline).
    ;; Point stays.
    [(< pt eol)
     (let* ([text (dirty-substring db pt eol)]
            [db1  (dirty-delete! db pt eol)])
       (kill-new text)
       (yank-mark-start! db1 pt))]
    ;; Point is at newline or end: kill the newline to join lines.
    [(< pt len)
     (let* ([next (gap-next-char-pos gb pt)]
            [db1  (dirty-delete! db pt next)])
       (kill-new "\n")
       (yank-mark-start! db1 pt))]
    [else db]))

;; ============================================================
;; Region — kill / delete / copy the active region
;; ============================================================

(define (cmd-kill-region db)
  ;; Kill text in the active region and push to kill-ring.
  (define buf (dirty-buffer-buf db))
  (if (region-active? buf)
      (let* ([beg  (region-beginning buf)]
             [end  (region-end buf)]
             [text (dirty-substring db beg end)]
             [db1  (dirty-delete! db beg end)]
             [db2  (dirty-set-point! db1 beg)])
        (kill-new text)
        (deactivate-mark! buf)
        db2)
      db))

(define (cmd-delete-region db)
  ;; Delete text in the active region without pushing to kill-ring.
  (define buf (dirty-buffer-buf db))
  (if (region-active? buf)
      (let* ([beg  (region-beginning buf)]
             [end  (region-end buf)]
             [db1  (dirty-delete! db beg end)]
             [db2  (dirty-set-point! db1 beg)])
        (deactivate-mark! buf)
        db2)
      db))

(define (cmd-copy-region db)
  ;; Copy text in the active region to kill-ring without deleting.
  (define buf (dirty-buffer-buf db))
  (if (region-active? buf)
      (let ([text (dirty-substring db (region-beginning buf) (region-end buf))])
        (kill-new text)
        (deactivate-mark! buf)
        db)
      db))

;; ============================================================
;; Yank / Paste
;; ============================================================

(define yank-start-box (box #f))  ;; position where last yank started

(define (yank-mark-start! db pos)
  ;; Record where the yank text begins (for yank-pop undo).
  (set-box! yank-start-box pos)
  db)

(define (cmd-yank db)
  (define text (kill-ring-yank))
  (if (and text (positive? (string-length text)))
      (let* ([pt   (dirty-point db)]
             [db1  (dirty-insert! db text pt)]
             [blen (bytes-length (string->bytes/utf-8 text))]
             [db2  (dirty-set-point! db1 (+ pt blen))])
        (set-box! yank-start-box pt)
        db2)
      db))

(define (cmd-yank-pop db)
  ;; Replace the just-yanked text with the previous kill-ring entry.
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
  ;; Commit pending records (for the command that was just executed),
  ;; then undo the most recent undo group.
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
      (dirty-set-point! db (gap-next-char-pos (buf-gap db) pt))
      db))

(define (cmd-backward-char db)
  (define pt (dirty-point db))
  (if (> pt 0)
      (dirty-set-point! db (gap-prev-char-pos (buf-gap db) pt))
      db))

;; ============================================================
;; Line Helpers (pure)
;; ============================================================

(define (line-beginning gb pt)
  ;; Return byte-position of the first character on the line containing pt.
  (let loop ([p (gap-prev-char-pos gb pt)])
    (if (zero? p)
        0
        (if (char=? (gap-char gb p) #\newline)
            (gap-next-char-pos gb p)
            (loop (gap-prev-char-pos gb p))))))

(define (line-end gb pt len)
  ;; Return byte-position of the newline ending the line containing pt
  ;; (or buffer-end if last line).
  (let loop ([p pt])
    (cond [(>= p len) len]
          [(char=? (gap-char gb p) #\newline) p]
          [else (loop (gap-next-char-pos gb p))])))

(define (display-column gb pt)
  ;; Visual column (0-based) of pt within its line.
  (gap-display-width gb (line-beginning gb pt) pt))

(define (move-to-column gb bol target-col len)
  ;; Return byte-pos at or before target-col on line starting at bol.
  ;; Clamped to line-end if the line is shorter than target-col.
  (define eol (line-end gb bol len))
  (scan-display-width gb bol eol target-col))

;; ============================================================
;; Cursor Movement — simple
;; ============================================================

(define (cmd-beginning-of-line db)
  (define pt (dirty-point db))
  (if (zero? pt)
      db
      (let ([bol (line-beginning (buf-gap db) pt)])
        (dirty-set-point! db bol))))

(define (cmd-end-of-line db)
  (define pt (dirty-point db))
  (define len (dirty-length db))
  (if (>= pt len)
      db
      (let ([eol (line-end (buf-gap db) pt len)])
        (dirty-set-point! db eol))))

(define (cmd-next-line db)
  ;; Move to next line, preserving visual column.
  (define gb  (buf-gap db))
  (define pt  (dirty-point db))
  (define len (dirty-length db))
  (define goal-col (display-column gb pt))
  (define eol (line-end gb pt len))
  (when (< eol len)
    (define next-bol (gap-next-char-pos gb eol))
    (dirty-set-point! db (move-to-column gb next-bol goal-col len)))
  db)

(define (cmd-prev-line db)
  ;; Move to previous line, preserving visual column.
  (define gb  (buf-gap db))
  (define pt  (dirty-point db))
  (if (zero? pt)
      db
      (let* ([len       (dirty-length db)]
             [bol       (line-beginning gb pt)]
             [goal-col  (display-column gb pt)])
        (if (zero? bol)
            (dirty-set-point! db 0)
            (let* ([prev-end (gap-prev-char-pos gb bol)]
                   [prev-bol (line-beginning gb prev-end)])
              (dirty-set-point! db (move-to-column gb prev-bol goal-col len))))
        db)))

;; ============================================================
;; Mark / Region
;; ============================================================

(define (cmd-set-mark db)
  (dirty-set-mark! db))

(define (cmd-swap-point-and-mark db)
  (define buf (dirty-buffer-buf db))
  (if (region-active? buf)
      (let* ([m    (buffer-mark buf)]
             [tx   (buffer-text buf)]
             [mp   (text-marker-pos tx m)]
             [pt   (dirty-point db)]
             [db1  (dirty-set-point! db mp)])
        (text-set-marker-pos! tx m pt)
        db1)
      db))
