#lang racket

;; kernel/key-event/key-event.rkt — Key event (pure abstract data)
;;
;; A key-event is the output of terminal decoding — a structured
;; description of one keypress.  It has NO knowledge of terminal
;; byte sequences (no #\rubout, no Ctrl-g char-ci=? hacks).
;; The platform/event layer is responsible for mapping raw bytes
;; to this abstract representation.
;;
;; Fields:
;;   char   — (or/c char? #f)  displayable character (letter/digit/punct)
;;   ctrl?  — boolean?         control modifier
;;   meta?  — boolean?         meta/alt modifier
;;   shift? — boolean?         shift modifier
;;   symbol — (or/c symbol? #f) named key (up/down/f1/escape/backspace...)

(provide
 key-event? key-event
 key-event-char key-event-ctrl? key-event-meta? key-event-shift?
 key-event-symbol

 key-symbol?
 self-insert-key? backspace-key? return-key? cancel-key?

 key-event->description)

;; ============================================================
;; Struct
;; ============================================================

(struct key-event
  (char     ; (or/c char? #f)
   ctrl?    ; boolean?
   meta?    ; boolean?
   shift?   ; boolean?
   symbol)  ; (or/c symbol? #f)
  #:transparent)

;; ============================================================
;; Known key symbols — all named keys the kernel recognises
;; ============================================================

(define known-symbols
  '(up down left right home end prior next insert delete
    backspace return tab escape cancel
    f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12
    menu
    kp-0 kp-1 kp-2 kp-3 kp-4 kp-5 kp-6 kp-7 kp-8 kp-9
    kp-add kp-subtract kp-multiply kp-divide kp-separator kp-equal
    kp-enter))

(define (key-symbol? v)
  (and (memq v known-symbols) #t))

;; ============================================================
;; Classification — pure symbol/field checks, no byte codes
;; ============================================================

(define (self-insert-key? ke)
  ;; Has a printable character (≥32, excluding DEL), no modifying flags.
  (define ch (key-event-char ke))
  (and ch
       (not (key-event-ctrl? ke))
       (not (key-event-meta? ke))
       (>= (char->integer ch) 32)
       (not (char=? ch #\rubout))))

(define (backspace-key? ke)
  (eq? (key-event-symbol ke) 'backspace))

(define (return-key? ke)
  (eq? (key-event-symbol ke) 'return))

(define (cancel-key? ke)
  ;; Escape or platform-mapped cancel.
  (or (eq? (key-event-symbol ke) 'escape)
      (eq? (key-event-symbol ke) 'cancel)))

;; ============================================================
;; Display
;; ============================================================

(define (key-event->description ke)
  (define parts '())
  (when (key-event-shift? ke) (set! parts (cons "S-" parts)))
  (when (key-event-meta? ke)  (set! parts (cons "M-" parts)))
  (when (key-event-ctrl? ke)  (set! parts (cons "C-" parts)))
  (cond
    [(key-event-symbol ke)
     (string-append (string-join parts "")
                    (symbol->string (key-event-symbol ke)))]
    [(key-event-char ke)
     (define ch (key-event-char ke))
     (string-append (string-join parts "")
                    (match ch
                      [#\space "SPC"]
                      [#\newline "RET"]
                      [#\tab "TAB"]
                      [_ (string ch)]))]
    [else "???"]))
