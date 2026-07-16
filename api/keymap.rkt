#lang racket

;; api/keymap.rkt — Composable keymaps with per-buffer override
;;
;; A keymap maps key → command.
;; Per-buffer local keymaps override the global keymap.
;; lookup-key searches local → global → #f.

(require "command.rkt"
         "key.rkt")

(provide
 ;; keymap operations
 make-keymap keymap-set! keymap-lookup
 ;; per-buffer keymaps
 buffer-keymap set-buffer-keymap!
 ;; global keymap
 global-keymap
 ;; composed lookup
 lookup-key
 ;; re-export key from key.rkt
 (all-from-out "key.rkt"))

;; ============================================================
;; Keymap
;; ============================================================

(define (make-keymap) (make-hash))
(define (keymap-set! km k cmd) (hash-set! km k cmd))
(define (keymap-lookup km k) (hash-ref km k (λ () #f)))

;; ============================================================
;; Per-buffer keymaps
;; ============================================================

(define buffer-keymap-table (make-hasheq))

(define (buffer-keymap buf)
  (hash-ref buffer-keymap-table buf (λ () #f)))

(define (set-buffer-keymap! buf km)
  (hash-set! buffer-keymap-table buf km))

;; ============================================================
;; Global keymap
;; ============================================================

(define global-keymap (make-keymap))

;; ============================================================
;; Composed lookup: local → global → #f
;; ============================================================

(define (lookup-key buf evt)
  (define k (key-event->key evt))
  (or (let ([local (buffer-keymap buf)])
        (and local (keymap-lookup local k)))
      (keymap-lookup global-keymap k)))
