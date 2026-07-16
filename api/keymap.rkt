#lang racket

;; api/keymap.rkt — Composable keymaps with per-buffer override
;;
;; A keymap maps (key-type . key-value) → command.
;; Multiple keymaps compose: later overrides earlier.
;; Each buffer can have a local keymap; fallback is the global keymap.

(require "command.rkt"
         "../kernel/key-event/key-event.rkt")

(provide
 ;; keymap type
 make-keymap keymap-set! keymap-lookup
 ;; key struct (for building bindings)
 key key->description
 ;; per-buffer keymaps
 buffer-keymap set-buffer-keymap!
 ;; composed lookup
 lookup-key
 ;; global keymap
 global-keymap
 ;; key extraction
 key-event->key)

;; ============================================================
;; Key struct — hash key for keymaps
;; ============================================================

(struct key (type value) #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc a b rec)
     (and (eq? (key-type a) (key-type b))
          (equal? (key-value a) (key-value b))))
   (define (hash-proc a rec)
     (equal-hash-code (cons (key-type a) (key-value a))))
   (define (hash2-proc a rec)
     (equal-secondary-hash-code (cons (key-type a) (key-value a))))])

(define (key-event->key evt)
  (cond [(key-event-symbol evt) (key 'symbol (key-event-symbol evt))]
        [(and (key-event-ctrl? evt) (key-event-char evt))
         (key 'ctrl (char-downcase (key-event-char evt)))]
        [(key-event-char evt) (key 'char (key-event-char evt))]
        [else (key 'symbol 'unknown)]))

(define (key->description k)
  (case (key-type k)
    [(symbol) (format "~a" (key-value k))]
    [(ctrl)   (format "C-~a" (key-value k))]
    [(char)   (format "~a" (key-value k))]
    [else     "unknown"]))

;; ============================================================
;; Keymap — just a hash table
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
;; Composed lookup: local → global → #f (self-insert fallback)
;; ============================================================
;; The event-loop calls this to find a command for a key event
;; in the current buffer's context.

(define (lookup-key buf evt)
  (define k (key-event->key evt))
  ;; Local keymap first
  (define local (buffer-keymap buf))
  (or (and local (keymap-lookup local k))
      ;; Then global
      (keymap-lookup global-keymap k)))
