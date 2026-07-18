#lang racket

;; edit.rkt — Unified editing commands (composition facade)
;;
;; Layers:
;;   kernel/base-edit.rkt   — 基础编辑 (字符/行级, 零 syntax 依赖)
;;   kernel/motion.rkt       — 纯扫描 (gap-buffer + syntax-table → position)
;;   kernel/data/syntax.rkt — syntax-table (纯数据)
;;
;; edit.rkt 的角色:
;;   - 对每个可变命令包裹 read-only 检查 (策略层)
;;   - 叠加 syntax-driven 高层命令 (word/sexp/symbol movement)
;;   - 不引入新数据结构, 只做纯组合
;;
;; kernel/ 不感知 read-only 策略, 也不依赖 syntax-table。
;; 这两个都是编辑策略, 在 edit.rkt 中组合。

(require (prefix-in base: "kernel/base-edit.rkt")   ; 基础命令 — 前缀导入
         "kernel/motion.rkt"                         ; 纯扫描
         "kernel/data/syntax.rkt"                    ; syntax-table
         "kernel/data/text.rkt"                      ; text-gap
         "kernel/dirty.rkt"                          ; dirty-*
         "kernel/buffer.rkt"                         ; buffer-read-only?
         "kernel/kill-ring.rkt")                     ; kill-new

;; ── read-only guard ─────────────────────────
;;
;; 可变命令在执行前检查 buffer 的 read-only? 标记。
;; 只读时返回 db 不变 (no-op), 上层 (main.rkt) 看到 acted?=#f,
;; 不会触发 syntax/bracket/rescan 等后续流程。
;;
;; 非可变命令 (movement, mark) 不需要检查。

(define (buf buf&db) (dirty-buffer-buf buf&db))

(define (guard-read-only db)
  (and (buffer-read-only? (buf db)) db))

;; ── 基础编辑命令 ──────────────────────────────
;;
;; 每个可变命令:
;;   1. guard-read-only → 只读则直接返回 db
;;   2. 否则调用 base:原始实现

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

;; ── 非可变命令 — 直接重导出 ────────────────────

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

;; ── 不检查版本 — 内部只读 buffer 写入用 ──────────
;; 签名和 checked 版本一致, 但不经过 guard-read-only。
;; error buffer, log buffer, display buffer 等程序化写入场景。

(define cmd-self-insert/unchecked    base:cmd-self-insert)
(define cmd-newline/unchecked        base:cmd-newline)
(define cmd-tab/unchecked            base:cmd-tab)
(define cmd-backward-delete/unchecked base:cmd-backward-delete)
(define cmd-forward-delete/unchecked  base:cmd-forward-delete)
(define cmd-kill-line/unchecked      base:cmd-kill-line)
(define cmd-yank/unchecked           base:cmd-yank)
(define cmd-yank-pop/unchecked       base:cmd-yank-pop)
(define cmd-undo/unchecked           base:cmd-undo)
(define cmd-redo/unchecked           base:cmd-redo)

;; ============================================================
;; Helpers
;; ============================================================

(define (buf-gap buf) (text-gap (buffer-text buf)))

;; Syntax-driven commands are no-ops when st is #f.

;; ============================================================
;; Word movement (non-mutating — no read-only guard needed)
;; ============================================================

(define (cmd-forward-word db [st #f])
  (if st
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (buf-gap buf)]
             [pt  (dirty-point db)]
             [len (dirty-length db)]
             [end (scan-word-forward gb pt len st)])
        (dirty-set-point! db end))
      db))

(define (cmd-backward-word db [st #f])
  (if st
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (buf-gap buf)]
             [pt  (dirty-point db)]
             [beg (scan-word-backward gb pt st)])
        (dirty-set-point! db beg))
      db))

;; ============================================================
;; Symbol movement (non-mutating)
;; ============================================================

(define (cmd-forward-symbol db [st #f])
  (if st
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (buf-gap buf)]
             [pt  (dirty-point db)]
             [len (dirty-length db)]
             [end (scan-symbol-forward gb pt len st)])
        (dirty-set-point! db end))
      db))

(define (cmd-backward-symbol db [st #f])
  (if st
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (buf-gap buf)]
             [pt  (dirty-point db)]
             [beg (scan-symbol-backward gb pt st)])
        (dirty-set-point! db beg))
      db))

;; ============================================================
;; Sexp movement (non-mutating)
;; ============================================================

(define (cmd-forward-sexp db [st #f])
  (if st
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (buf-gap buf)]
             [pt  (dirty-point db)]
             [len (dirty-length db)]
             [end (scan-sexp-forward gb pt len st)])
        (dirty-set-point! db end))
      db))

(define (cmd-backward-sexp db [st #f])
  (if st
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (buf-gap buf)]
             [pt  (dirty-point db)]
             [beg (scan-sexp-backward gb pt st)])
        (dirty-set-point! db beg))
      db))

;; ============================================================
;; Kill commands — mutating, need read-only guard
;; ============================================================

(define (kill-word-impl db st)
  (let* ([buf   (dirty-buffer-buf db)]
         [gb    (buf-gap buf)]
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
  (let* ([buf   (dirty-buffer-buf db)]
         [gb    (buf-gap buf)]
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
  (let* ([buf   (dirty-buffer-buf db)]
         [gb    (buf-gap buf)]
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
;; General char-class skip (non-mutating)
;; ============================================================

(define (cmd-skip-chars-forward db class [st #f])
  (if st
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (buf-gap buf)]
             [pt  (dirty-point db)]
             [len (dirty-length db)]
             [end (skip-char-class-forward gb pt len st class)])
        (dirty-set-point! db end))
      db))

(define (cmd-skip-chars-backward db class [st #f])
  (if st
      (let* ([buf (dirty-buffer-buf db)]
             [gb  (buf-gap buf)]
             [pt  (dirty-point db)]
             [beg (skip-char-class-backward gb pt st class)])
        (dirty-set-point! db beg))
      db))

;; ── 对外 provide ──────────────────────────────
(provide
 ;; content-modifying (all guarded)
 cmd-self-insert cmd-newline cmd-tab
 cmd-backward-delete cmd-forward-delete
 cmd-kill-line
 cmd-yank cmd-yank-pop
 cmd-undo cmd-redo

 ;; content-modifying — unchecked (internal / read-only buffer 写入)
 cmd-self-insert/unchecked cmd-newline/unchecked cmd-tab/unchecked
 cmd-backward-delete/unchecked cmd-forward-delete/unchecked
 cmd-kill-line/unchecked
 cmd-yank/unchecked cmd-yank-pop/unchecked
 cmd-undo/unchecked cmd-redo/unchecked

 ;; syntax-driven (kill variants guarded)
 cmd-kill-word cmd-backward-kill-word
 cmd-kill-sexp

 ;; syntax-driven — unchecked (internal 写入)
 cmd-kill-word/unchecked cmd-backward-kill-word/unchecked
 cmd-kill-sexp/unchecked

 ;; movement / mark (no guard needed)
 cmd-forward-char cmd-backward-char
 cmd-beginning-of-line cmd-end-of-line
 cmd-next-line cmd-prev-line
 line-beginning line-end
 display-column move-to-column
 cmd-set-mark cmd-swap-point-and-mark

 ;; syntax-driven movement (no guard needed)
 cmd-forward-word cmd-backward-word
 cmd-forward-symbol cmd-backward-symbol
 cmd-forward-sexp cmd-backward-sexp
 cmd-skip-chars-forward cmd-skip-chars-backward)
