#lang racket

;; lang/syntax.rkt — Re-exports kernel/data/syntax.rkt
;;
;; This is a compatibility shim.  The canonical syntax-table definition
;; lives in kernel/data/syntax.rkt.  All lang modules should require
;; this file (or kernel/data/syntax.rkt directly).

(require "../kernel/data/syntax.rkt")

(provide (all-from-out "../kernel/data/syntax.rkt"))
