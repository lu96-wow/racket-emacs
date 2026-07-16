#lang racket

;; api/keymap.rkt — Composable keymaps
;;
;; A keymap maps key → (or/c command? keymap?).
;; lookup-key searches local → global → #f.

(require "command.rkt"
         "key.rkt")

(provide
 ;; keymap
 make-keymap keymap-set! keymap-lookup
 keymap? keymap-hash
 ;; buffer keymaps
 buffer-keymap set-buffer-keymap!
 ;; global
 global-keymap
 ;; lookup
 lookup-key
 ;; helpers
 keymap-value-command? keymap-value-keymap?
 ;; re-export
 (all-from-out "key.rkt"))

;; ============================================================
;; Keymap
;; ============================================================

(struct keymap (hash) #:transparent)

(define (make-keymap) (keymap (make-hash)))
(define (keymap-set! km k v) (hash-set! (keymap-hash km) k v))
(define (keymap-lookup km k) (hash-ref (keymap-hash km) k (λ () #f)))
(define (keymap-value-command? v) (command? v))
(define (keymap-value-keymap? v) (keymap? v))

;; ============================================================
;; Buffer keymaps
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
;; lookup-key: local → global → #f
;; ============================================================

(define (lookup-key buf evt)
  (define k (key-event->key evt))
  (or (let ([local (buffer-keymap buf)])
        (and local (keymap-lookup local k)))
      (keymap-lookup global-keymap k)))
