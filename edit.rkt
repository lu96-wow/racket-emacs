#lang racket

;; edit.rkt — Unified editing commands (composition facade)
;;
;; ============================================================================
;; Layers:
;;   kernel/base-edit.rkt   — basic editing (char/line level, zero syntax deps)
;;   kernel/motion.rkt       — pure scanning (gap-buffer + syntax-table → pos)
;;   kernel/data/syntax.rkt — syntax-table (pure data)
;;
;; edit.rkt does two things:
;;   1. Wraps every content-modifying command with read-only guard
;;   2. Overlays syntax-driven higher-level commands (word/sexp/symbol movement)
;;
;; kernel/ never sees read-only policy or syntax-table — both are editing
;; strategy, composed here.
;;
;; ============================================================================
;; Computation vs Application
;; ============================================================================
;;
;;   All commands are APPLICATION — they take a dirty-buffer and return
;;   a (possibly new) dirty-buffer.  Content-modifying variants are guarded
;;   by read-only check; movement commands pass through without guard.
;;
;; ============================================================================
;; Guard Pattern
;; ============================================================================
;;
;;   (or (guard-read-only db)     ;; returns db (truthy) if read-only
;;       (base:cmd-fn db ...))    ;; otherwise execute the command
;;
;;   When read-only: `or` short-circuits → returns original db (no-op)
;;   When writable:  guard returns #f → `or` evaluates command → new db
;;
;; ============================================================================

(require (prefix-in base: "kernel/base-edit.rkt")
         "kernel/motion.rkt"
         "kernel/data/syntax.rkt"
         "kernel/data/text.rkt"
         "kernel/dirty.rkt"
         "kernel/buffer.rkt"
         "kernel/kill-ring.rkt")

;; ============================================================
;; Helpers
;; ============================================================

(define (buf db) (dirty-buffer-buf db))
(define (buf-gap db) (text-gap (buffer-text (dirty-buffer-buf db))))

(define (guard-read-only db)
  ;; Returns db (truthy) if buffer is read-only, otherwise #f.
  ;; Used with `or` to short-circuit: (or (guard-read-only db) (mutate db)).
  (and (buffer-read-only? (dirty-buffer-buf db)) db))

;; ============================================================
;; Content-Modifying Commands — guarded
;; ============================================================

(define (cmd-self-insert db ch)
  (or (guard-read-only db)
      (base:cmd-self-insert db ch)))

(define (cmd-newline db)
  (or (guard-read-only db)
      (base:cmd-newline db)))

(define (cmd-tab db)
  (or (guard-read-only db)
      (base:cmd-tab db)))

(define (cmd-backward-delete db)
  (or (guard-read-only db)
      (base:cmd-backward-delete db)))

(define (cmd-forward-delete db)
  (or (guard-read-only db)
      (base:cmd-forward-delete db)))

(define (cmd-kill-line db)
  (or (guard-read-only db)
      (base:cmd-kill-line db)))

(define (cmd-kill-region db)
  ;; Kill (cut) the active region to kill-ring.
  (or (guard-read-only db)
      (base:cmd-kill-region db)))

(define (cmd-delete-region db)
  ;; Delete the active region without pushing to kill-ring.
  (or (guard-read-only db)
      (base:cmd-delete-region db)))

(define (cmd-copy-region db)
  ;; Copy the active region to kill-ring without deleting.
  (or (guard-read-only db)
      (base:cmd-copy-region db)))

(define (cmd-yank db)
  (or (guard-read-only db)
      (base:cmd-yank db)))

(define (cmd-yank-pop db)
  (or (guard-read-only db)
      (base:cmd-yank-pop db)))

(define (cmd-undo db)
  (or (guard-read-only db)
      (base:cmd-undo db)))

(define (cmd-redo db)
  (or (guard-read-only db)
      (base:cmd-redo db)))

;; ============================================================
;; Non-Mutating Commands — direct pass-through (no guard needed)
;; ============================================================

(define cmd-forward-char      base:cmd-forward-char)
(define cmd-backward-char     base:cmd-backward-char)
(define cmd-beginning-of-line base:cmd-beginning-of-line)
(define cmd-end-of-line       base:cmd-end-of-line)
(define cmd-next-line         base:cmd-next-line)
(define cmd-prev-line         base:cmd-prev-line)
(define line-beginning        base:line-beginning)
(define line-end              base:line-end)
(define display-column        base:display-column)
(define move-to-column        base:move-to-column)
(define cmd-set-mark          base:cmd-set-mark)
(define cmd-swap-point-and-mark base:cmd-swap-point-and-mark)

;; ============================================================
;; Unchecked Variants — for internal writes to read-only buffers
;; ============================================================
;; Used by error buffer, log buffer, display buffer, etc.
;; Same signatures as guarded versions, without the guard.

(define cmd-self-insert/unchecked    base:cmd-self-insert)
(define cmd-newline/unchecked        base:cmd-newline)
(define cmd-tab/unchecked            base:cmd-tab)
(define cmd-backward-delete/unchecked base:cmd-backward-delete)
(define cmd-forward-delete/unchecked  base:cmd-forward-delete)
(define cmd-kill-line/unchecked      base:cmd-kill-line)
(define cmd-kill-region/unchecked    base:cmd-kill-region)
(define cmd-delete-region/unchecked  base:cmd-delete-region)
(define cmd-copy-region/unchecked    base:cmd-copy-region)
(define cmd-yank/unchecked           base:cmd-yank)
(define cmd-yank-pop/unchecked       base:cmd-yank-pop)
(define cmd-undo/unchecked           base:cmd-undo)
(define cmd-redo/unchecked           base:cmd-redo)

;; ============================================================
;; Syntax-Driven Movement — no guard needed (non-mutating)
;; ============================================================

(define (cmd-forward-word db [st #f])
  ;; Move forward to end of current/next word.
  ;; No-op if st is #f (no syntax-table).
  (if st
      (let* ([gb  (buf-gap db)]
             [pt  (dirty-point db)]
             [len (dirty-length db)]
             [end (scan-word-forward gb pt len st)])
        (dirty-set-point! db end))
      db))

(define (cmd-backward-word db [st #f])
  (if st
      (let* ([gb  (buf-gap db)]
             [pt  (dirty-point db)]
             [beg (scan-word-backward gb pt st)])
        (dirty-set-point! db beg))
      db))

(define (cmd-forward-symbol db [st #f])
  (if st
      (let* ([gb  (buf-gap db)]
             [pt  (dirty-point db)]
             [len (dirty-length db)]
             [end (scan-symbol-forward gb pt len st)])
        (dirty-set-point! db end))
      db))

(define (cmd-backward-symbol db [st #f])
  (if st
      (let* ([gb  (buf-gap db)]
             [pt  (dirty-point db)]
             [beg (scan-symbol-backward gb pt st)])
        (dirty-set-point! db beg))
      db))

(define (cmd-forward-sexp db [st #f])
  (if st
      (let* ([gb  (buf-gap db)]
             [pt  (dirty-point db)]
             [len (dirty-length db)]
             [end (scan-sexp-forward gb pt len st)])
        (dirty-set-point! db end))
      db))

(define (cmd-backward-sexp db [st #f])
  (if st
      (let* ([gb  (buf-gap db)]
             [pt  (dirty-point db)]
             [beg (scan-sexp-backward gb pt st)])
        (dirty-set-point! db beg))
      db))

(define (cmd-skip-chars-forward db class [st #f])
  (if st
      (let* ([gb  (buf-gap db)]
             [pt  (dirty-point db)]
             [len (dirty-length db)]
             [end (skip-char-class-forward gb pt len st class)])
        (dirty-set-point! db end))
      db))

(define (cmd-skip-chars-backward db class [st #f])
  (if st
      (let* ([gb  (buf-gap db)]
             [pt  (dirty-point db)]
             [beg (skip-char-class-backward gb pt st class)])
        (dirty-set-point! db beg))
      db))

;; ============================================================
;; Syntax-Driven Kill Commands — guarded
;; ============================================================

(define (kill-word-impl db st)
  (let* ([gb    (buf-gap db)]
         [start (dirty-point db)]
         [len   (dirty-length db)]
         [end   (scan-word-forward gb start len st)])
    (if (> end start)
        (let* ([text (dirty-substring db start end)]
               [db1  (dirty-delete! db start end)])
          (kill-new text)
          db1)
        db)))

(define (cmd-kill-word db [st #f])
  (or (guard-read-only db)
      (and st (kill-word-impl db st))
      db))

(define (cmd-kill-word/unchecked db [st #f])
  (if st (kill-word-impl db st) db))

(define (kill-backward-word-impl db st)
  (let* ([gb    (buf-gap db)]
         [end   (dirty-point db)]
         [start (scan-word-backward gb end st)])
    (if (< start end)
        (let* ([text (dirty-substring db start end)]
               [db1  (dirty-delete! db start end)])
          (kill-new text)
          (dirty-set-point! db1 start))
        db)))

(define (cmd-backward-kill-word db [st #f])
  (or (guard-read-only db)
      (and st (kill-backward-word-impl db st))
      db))

(define (cmd-backward-kill-word/unchecked db [st #f])
  (if st (kill-backward-word-impl db st) db))

(define (kill-sexp-impl db st)
  (let* ([gb    (buf-gap db)]
         [start (dirty-point db)]
         [len   (dirty-length db)]
         [end   (scan-sexp-forward gb start len st)])
    (if (> end start)
        (let* ([text (dirty-substring db start end)]
               [db1  (dirty-delete! db start end)])
          (kill-new text)
          db1)
        db)))

(define (cmd-kill-sexp db [st #f])
  (or (guard-read-only db)
      (and st (kill-sexp-impl db st))
      db))

(define (cmd-kill-sexp/unchecked db [st #f])
  (if st (kill-sexp-impl db st) db))

;; ============================================================
;; Provide — all commands
;; ============================================================

(provide
 ;; ── content-modifying (guarded) ──
 cmd-self-insert cmd-newline cmd-tab
 cmd-backward-delete cmd-forward-delete
 cmd-kill-line
 cmd-kill-region cmd-delete-region cmd-copy-region
 cmd-yank cmd-yank-pop
 cmd-undo cmd-redo

 ;; ── content-modifying — unchecked (internal / read-only buffer writes) ──
 cmd-self-insert/unchecked cmd-newline/unchecked cmd-tab/unchecked
 cmd-backward-delete/unchecked cmd-forward-delete/unchecked
 cmd-kill-line/unchecked
 cmd-kill-region/unchecked cmd-delete-region/unchecked
 cmd-copy-region/unchecked
 cmd-yank/unchecked cmd-yank-pop/unchecked
 cmd-undo/unchecked cmd-redo/unchecked

 ;; ── syntax-driven kill (guarded) ──
 cmd-kill-word cmd-backward-kill-word
 cmd-kill-sexp

 ;; ── syntax-driven kill — unchecked ──
 cmd-kill-word/unchecked cmd-backward-kill-word/unchecked
 cmd-kill-sexp/unchecked

 ;; ── movement / mark (no guard needed) ──
 cmd-forward-char cmd-backward-char
 cmd-beginning-of-line cmd-end-of-line
 cmd-next-line cmd-prev-line
 line-beginning line-end
 display-column move-to-column
 cmd-set-mark cmd-swap-point-and-mark

 ;; ── syntax-driven movement (no guard needed) ──
 cmd-forward-word cmd-backward-word
 cmd-forward-symbol cmd-backward-symbol
 cmd-forward-sexp cmd-backward-sexp
 cmd-skip-chars-forward cmd-skip-chars-backward)
