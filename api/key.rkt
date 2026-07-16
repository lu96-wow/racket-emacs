#lang racket

;; api/key.rkt — Key abstraction
;;
;; A key is a normalized representation of a keyboard event.
;; Used as hash keys in keymaps.  Deduplicated from kernel/key-event.

(require "../kernel/key-event/key-event.rkt")

(provide
 ;; key
 key? key key-prefix key-suffix
 ;; conversion
 key-event->key
 ;; built-in checks
 self-insert-key?
 ;; re-export key-event types
 key-event? key-event-char key-event-ctrl? key-event-meta? key-event-shift?
 key-event-symbol
 mouse-event? mouse-event
 mouse-event-button mouse-event-x mouse-event-y mouse-event-action
 mouse-event-shift? mouse-event-alt? mouse-event-ctrl?
 input-event? key-symbol?)

;; ============================================================
;; Key — normalized key representation
;; ============================================================

(struct key (prefix suffix) #:transparent)

(define (key-event->key ke)
  (cond [(key-event-symbol ke)
         (define modifiers
           (append (if (key-event-ctrl? ke) '(ctrl) '())
                   (if (key-event-meta? ke) '(meta) '())
                   (if (key-event-shift? ke) '(shift) '())))
         (if (null? modifiers)
             (key 'symbol (key-event-symbol ke))
             (key 'symbol (cons (key-event-symbol ke) modifiers)))]
        [(key-event-char ke)
         (cond [(and (key-event-ctrl? ke) (key-event-char ke))
                (key 'ctrl (char-downcase (key-event-char ke)))]
               [(key-event-meta? ke)
                (key 'meta (key-event-char ke))]
               [else (key 'char (key-event-char ke))])]
        [else (key 'unknown #f)]))

(define (self-insert-key? ke)
  (and (key-event? ke)
       (let ([ch (key-event-char ke)])
         (and ch
              (not (key-event-ctrl? ke))
              (not (key-event-meta? ke))
              (>= (char->integer ch) 32)
              (not (char=? ch #\rubout))))))
