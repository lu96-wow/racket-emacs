#lang racket

;; core/keymap.rkt — Sparse prefix tree keymap
;;
;; Pure operations on key-event sequences.
;; String→key-event parsing is in modes/keybind.rkt.

(require "key-event.rkt")

(provide
 ;; re-export from key-event
 (all-from-out "key-event.rkt")

 ;; keymap
 make-keymap keymap? keymap-bindings keymap-parent
 set-keymap-parent!

 ;; operations
 define-key lookup-key

 ;; per-buffer keymap state
 set-buffer-keymap! buffer-keymap

 ;; global keymap + buffer-aware lookup
 global-keymap buffer-lookup-key

 ;; cleanup
 keymap-buffer-cleanup!

 ;; echo helper
 key-sequence->echo-string)

;; ============================================================
;; Keymap
;; ============================================================

(struct keymap
  ([bindings #:mutable]  ; (hash/c key-event? (or/c keymap? procedure?))
   [parent #:mutable])   ; keymap? | #f
  #:transparent)

(define (make-keymap [parent #f])
  (keymap (make-hash) parent))

;; ============================================================
;; Hash key normalization — C-a ↔ #\x01
;; ============================================================

(define (key-event->hash-key ke)
  (cond
    [(and (key-event-ctrl? ke) (key-event-char ke)
          (char-alphabetic? (key-event-char ke)))
     (key-event (integer->char
                 (- (char->integer (char-downcase (key-event-char ke)))
                    (char->integer #\a) -1))
                #f (key-event-meta? ke) (key-event-shift? ke) #f)]
    [(key-event-symbol ke)
     (key-event #f (key-event-ctrl? ke) (key-event-meta? ke)
                (key-event-shift? ke) (key-event-symbol ke))]
    [else ke]))

;; ============================================================
;; define-key / lookup-key
;; ============================================================

(define (define-key km key-sequence def)
  (match key-sequence
    [(list ke)
     (hash-set! (keymap-bindings km) (key-event->hash-key ke) def)]
    [(list ke rest ...)
     (define sub-km
       (hash-ref (keymap-bindings km) (key-event->hash-key ke)
         (λ ()
           (define new-km (make-keymap))
           (hash-set! (keymap-bindings km) (key-event->hash-key ke) new-km)
           new-km)))
     (unless (keymap? sub-km)
       (error 'define-key "~a already bound to non-keymap"
              (key-event->description ke)))
     (define-key sub-km rest def)]
    [_ (error 'define-key "invalid key sequence: ~a" key-sequence)]))

(define (lookup-key km key-sequence)
  (lookup-key* km key-sequence #t))

(define (lookup-key* km key-sequence allow-parent?)
  (match key-sequence
    ['() km]
    [(list ke)
     (define key (key-event->hash-key ke))
     (define binding (hash-ref (keymap-bindings km) key (λ () #f)))
     (or binding
         (and allow-parent? (keymap-parent km)
              (lookup-key* (keymap-parent km) key-sequence allow-parent?)))]
    [(list ke rest ...)
     (define key (key-event->hash-key ke))
     (define sub (hash-ref (keymap-bindings km) key (λ () #f)))
     (cond [(keymap? sub) (lookup-key* sub rest allow-parent?)]
           [(and allow-parent? (keymap-parent km))
            (lookup-key* (keymap-parent km) key-sequence allow-parent?)]
           [else #f])]))

;; ============================================================
;; Echo helper
;; ============================================================

(define (key-sequence->echo-string keys)
  (string-join (map key-event->description keys) " "))

;; ============================================================
;; Per-buffer keymap state
;; ============================================================

(define keymap-table (make-hasheq))
(define (set-buffer-keymap! buf km) (hash-set! keymap-table buf km))
(define (buffer-keymap buf) (hash-ref keymap-table buf (λ () #f)))

;; ============================================================
;; Global keymap + buffer-aware lookup
;; ============================================================

(define global-keymap (make-keymap))

(define (buffer-lookup-key buf keys)
  (define km (buffer-keymap buf))
  (define b (and km (lookup-key km keys)))
  (or b (lookup-key global-keymap keys)))

;; ============================================================
;; Cleanup
;; ============================================================

(define (keymap-buffer-cleanup! buf)
  (hash-remove! keymap-table buf))
