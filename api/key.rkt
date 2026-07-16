#lang racket

;; api/key.rkt — Key struct for hash-based dispatch
;;
;; The `key` struct maps terminal events to hash keys for keymap lookup.
;; `key-event->key` converts a key-event to a key.

(require "../kernel/key-event/key-event.rkt")

(provide
 key? key
 key-type key-value
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

;; ============================================================
;; key-event → key
;; ============================================================

(define (key-event->key evt)
  (cond [(key-event-symbol evt) (key 'symbol (key-event-symbol evt))]
        [(and (key-event-ctrl? evt) (key-event-char evt))
         (key 'ctrl (char-downcase (key-event-char evt)))]
        [(key-event-char evt) (key 'char (key-event-char evt))]
        [else (key 'symbol 'unknown)]))
