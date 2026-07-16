#lang racket

;; api/keymap.rkt — Composable keymaps with per-buffer override
;;
;; A keymap maps key → (or/c command? keymap?).
;; Nested keymaps implement prefix keys (e.g. C-x → prefix keymap).
;; Per-buffer local keymaps override the global keymap.
;; lookup-key searches local → global → #f.

(require "command.rkt"
         "key.rkt")

(provide
 ;; keymap operations
 make-keymap keymap-set! keymap-lookup
 keymap? keymap-hash
 ;; per-buffer keymaps
 buffer-keymap set-buffer-keymap!
 ;; global keymap
 global-keymap
 ;; composed lookup
 lookup-key
 ;; prefix helpers
 keymap-value-command? keymap-value-keymap?
 ;; re-export key from key.rkt
 (all-from-out "key.rkt"))

;; ============================================================
;; Keymap
;; ============================================================

;; A keymap is a hash: key → (or/c command? (hash key → ...))
;; We use a transparent struct wrapper for type-safety.
(struct keymap (hash) #:transparent)

(define (make-keymap) (keymap (make-hash)))

(define (keymap-set! km k v)
  (hash-set! (keymap-hash km) k v))

(define (keymap-lookup km k)
  (hash-ref (keymap-hash km) k (λ () #f)))

;; Predicates for dispatch: a keymap value is either a command or a prefix keymap
(define (keymap-value-command? v) (command? v))
(define (keymap-value-keymap? v) (keymap? v))

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
