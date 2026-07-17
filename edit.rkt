#lang racket

;; edit.rkt — Unified editing commands (composition facade)
;;
;; Layers:
;;   kernel/base-edit.rkt   — 基础编辑 (字符/行级, 零 syntax 依赖)
;;   kernel/motion.rkt       — 纯扫描 (gap-buffer + syntax-table → position)
;;   kernel/data/syntax.rkt — syntax-table (纯数据)
;;
;; edit.rkt 的角色:
;;   - 全量重导出 kernel/base-edit.rkt 的基础 API
;;   - 叠加 syntax-driven 高层命令 (word/sexp/symbol movement)
;;   - 不引入新数据结构, 只做纯组合
;;
;; kernel/ 永远不 import syntax-table; syntax 逻辑隔离在 edit.rkt 和 motion.rkt 中。

(require "kernel/base-edit.rkt"        ; 基础命令 — 全量透传
         "kernel/motion.rkt"           ; 纯扫描
         "kernel/data/syntax.rkt"      ; syntax-table
         "kernel/data/text.rkt"        ; text-gap
         "kernel/dirty.rkt"            ; dirty-*
         "kernel/buffer.rkt"           ; buffer-text
         "kernel/kill-ring.rkt")       ; kill-new

;; ── 底层全量透传 ──────────────────────────────
(provide (all-from-out "kernel/base-edit.rkt"))

;; ── 高层 syntax-driven API ───────────────────
(provide
 ;; word
 cmd-forward-word cmd-backward-word
 cmd-kill-word cmd-backward-kill-word

 ;; symbol
 cmd-forward-symbol cmd-backward-symbol

 ;; sexp
 cmd-forward-sexp cmd-backward-sexp
 cmd-kill-sexp

 ;; general char-class skip
 cmd-skip-chars-forward cmd-skip-chars-backward)

;; ============================================================
;; Helpers
;; ============================================================

(define (buf-gap buf) (text-gap (buffer-text buf)))

;; When st is #f, syntax-driven operations are no-ops:
;; the command returns db unchanged.

;; ============================================================
;; Word movement
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
;; Symbol movement
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
;; Sexp movement
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
;; Kill commands — motion + dirty-delete! + kill-new
;; ============================================================

(define (cmd-kill-word db [st #f])
  (if st
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
            db))
      db))

(define (cmd-backward-kill-word db [st #f])
  (if st
      (let* ([buf   (dirty-buffer-buf db)]
             [gb    (buf-gap buf)]
             [end   (dirty-point db)]
             [start (scan-word-backward gb end st)])
        (if (< start end)
            (let* ([text (dirty-substring db start end)]
                   [db1  (dirty-delete! db start end)])
              (kill-new text)
              (dirty-set-point! db1 start))
            db))
      db))

(define (cmd-kill-sexp db [st #f])
  (if st
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
            db))
      db))

;; ============================================================
;; General char-class skip
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
