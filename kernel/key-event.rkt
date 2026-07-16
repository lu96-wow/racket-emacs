#lang racket

;; kernel/key-event/key-event.rkt — Key event + mouse event (pure abstract data)
;;
;; Two event types:
;;   key-event   — keyboard input
;;   mouse-event — mouse actions (separate struct, never conflated with keys)
;;
;; Fields:
;;   key-event: char, ctrl?, meta?, shift?, symbol
;;   mouse-event: button, x, y, action, shift?, alt?, ctrl?

(provide
 key-event? key-event
 key-event-char key-event-ctrl? key-event-meta? key-event-shift?
 key-event-symbol

 mouse-event? mouse-event
 mouse-event-button mouse-event-x mouse-event-y
 mouse-event-action mouse-event-shift? mouse-event-alt? mouse-event-ctrl?

 input-event?
 key-symbol?
 self-insert-key? backspace-key? return-key? cancel-key?

 key-event->description mouse-event->description)

;; ============================================================
;; Structs
;; ============================================================

(struct key-event
  (char     ; (or/c char? #f)
   ctrl?    ; boolean?
   meta?    ; boolean?
   shift?   ; boolean?
   symbol)  ; (or/c symbol? #f)
  #:transparent)

(struct mouse-event
  (button   ; 'left 'middle 'right 'wheel-up 'wheel-down
   x        ; 0-based column
   y        ; 0-based row
   action   ; 'press 'release 'scroll-up 'scroll-down
   shift?   ; boolean?
   alt?     ; boolean?
   ctrl?)   ; boolean?
  #:transparent)

(define (input-event? x) (or (key-event? x) (mouse-event? x)))

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
  ;; Returns #f for non-key-events (mouse, etc.).
  (and (key-event? ke)
       (let ([ch (key-event-char ke)])
         (and ch
              (not (key-event-ctrl? ke))
              (not (key-event-meta? ke))
              (>= (char->integer ch) 32)
              (not (char=? ch #\rubout))))))

(define (backspace-key? ke)
  (and (key-event? ke) (eq? (key-event-symbol ke) 'backspace)))

(define (return-key? ke)
  (and (key-event? ke) (eq? (key-event-symbol ke) 'return)))

(define (cancel-key? ke)
  (and (key-event? ke)
       (or (eq? (key-event-symbol ke) 'escape)
           (eq? (key-event-symbol ke) 'cancel))))

;; ============================================================
;; Display
;; ============================================================

(define (key-event->description ke)
  (cond [(mouse-event? ke) (mouse-event->description ke)]
        [else
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
           [else "???"])]))

(define (mouse-event->description me)
  (format "mouse-~a ~a (~a,~a)"
          (mouse-event-button me)
          (mouse-event-action me)
          (mouse-event-x me)
          (mouse-event-y me)))
