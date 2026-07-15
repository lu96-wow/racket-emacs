#lang racket

;; protocol/keymap/keymap.rkt — Sparse prefix-tree keymap
;;
;; Pure operations on key-event sequences.
;; Depends on kernel/key-event for the event type.

(require "../kernel/key-event/key-event.rkt")

(provide
 ;; struct
 keymap? make-keymap
 keymap-bindings keymap-parent
 set-keymap-parent!

 ;; operations
 define-key
 lookup-key

 ;; display
 key-sequence->description)

;; ============================================================
;; Struct
;; ============================================================

(struct keymap
  ([bindings #:mutable]  ; (hash/c key-event? (or/c keymap? procedure?))
   [parent #:mutable])   ; (or/c keymap? #f)
  #:transparent)

(define (make-keymap [parent #f])
  (keymap (make-hash) parent))

;; ============================================================
;; Key normalization — Ctrl-letter ↔ control character
;; ============================================================
;; Both (C-a) and (#\x01) should map to the same binding.
;; We normalize Ctrl+alpha to the equivalent control byte.

(define (normalize-key ke)
  (cond
    [(and (key-event-ctrl? ke) (key-event-char ke)
          (char-alphabetic? (key-event-char ke)))
     ;; C-a → char=1, ctrl=#f
     (key-event (integer->char
                 (+ (char->integer (char-downcase (key-event-char ke)))
                    (- (char->integer #\a) -1)))
                #f
                (key-event-meta? ke)
                (key-event-shift? ke)
                #f)]
    [(key-event-symbol ke)
     ;; Named keys: drop char field
     (key-event #f
                (key-event-ctrl? ke)
                (key-event-meta? ke)
                (key-event-shift? ke)
                (key-event-symbol ke))]
    [else ke]))

;; ============================================================
;; define-key
;; ============================================================

(define (define-key km key-sequence def)
  (match key-sequence
    [(list ke)
     ;; Leaf: store definition
     (hash-set! (keymap-bindings km) (normalize-key ke) def)]
    [(list ke rest ...)
     ;; Prefix: walk into or create sub-keymap
     (define sub-km
       (hash-ref (keymap-bindings km) (normalize-key ke)
         (λ ()
           (define new-km (make-keymap))
           (hash-set! (keymap-bindings km) (normalize-key ke) new-km)
           new-km)))
     (unless (keymap? sub-km)
       (error 'define-key "~a already bound to a command, cannot extend"
              (key-event->description ke)))
     (define-key sub-km rest def)]
    [_ (error 'define-key "invalid key sequence: ~a" key-sequence)]))

;; ============================================================
;; lookup-key
;; ============================================================

(define (lookup-key km key-sequence)
  (lookup-key* km key-sequence #t))

(define (lookup-key* km key-sequence use-parent?)
  (match key-sequence
    ['() km]
    [(list ke)
     (define key (normalize-key ke))
     (define binding (hash-ref (keymap-bindings km) key (λ () #f)))
     (or binding
         (and use-parent? (keymap-parent km)
              (lookup-key* (keymap-parent km) key-sequence use-parent?)))]
    [(list ke rest ...)
     (define key (normalize-key ke))
     (define sub (hash-ref (keymap-bindings km) key (λ () #f)))
     (cond [(keymap? sub) (lookup-key* sub rest use-parent?)]
           [(and use-parent? (keymap-parent km))
            (lookup-key* (keymap-parent km) key-sequence use-parent?)]
           [else #f])]))

;; ============================================================
;; Display
;; ============================================================

(define (key-sequence->description keys)
  (string-join (map key-event->description keys) " "))
