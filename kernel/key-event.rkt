#lang racket

;; core/key-event.rkt — Key event struct & classification (pure data)

(provide
 ;; struct
 key-event? key-event
 key-event-char key-event-ctrl? key-event-meta? key-event-shift?
 key-event-symbol
 key-symbol?

 ;; classification
 key-event-self-insert? key-event-backspace? key-event-return?
 key-event-cancel?

 ;; display helper (pure data→string)
 key-event->description)

;; ============================================================
;; Struct
;; ============================================================

(struct key-event
  (char    ; char | #f
   ctrl?   ; boolean
   meta?   ; boolean
   shift?  ; boolean
   symbol) ; symbol | #f
  #:transparent)

(define (key-symbol? v)
  (memq v '(up down left right home end
            prior next insert delete
            f1 f2 f3 f4 f5 f6 f7 f8 f9 f10 f11 f12
            escape return backspace tab
            menu
            kp-0 kp-1 kp-2 kp-3 kp-4 kp-5 kp-6 kp-7 kp-8 kp-9
            kp-add kp-subtract kp-multiply kp-divide kp-separator kp-equal
            kp-enter)))

;; ============================================================
;; Classification
;; ============================================================

(define (key-event-self-insert? ke)
  (define ch (key-event-char ke))
  (and ch (not (key-event-ctrl? ke))
       (not (key-event-meta? ke))
       (>= (char->integer ch) 32)
       (not (char=? ch #\rubout))))

(define (key-event-backspace? ke)
  (or (and (key-event-symbol ke) (eq? (key-event-symbol ke) 'backspace))
      (and (key-event-char ke) (char=? (key-event-char ke) #\rubout))))

(define (key-event-return? ke)
  (and (key-event-symbol ke) (eq? (key-event-symbol ke) 'return)))

(define (key-event-cancel? ke)
  (or (and (key-event-ctrl? ke) (key-event-char ke)
           (char-ci=? (key-event-char ke) #\g))
      (and (key-event-symbol ke) (eq? (key-event-symbol ke) 'escape))))

;; ============================================================
;; Display helper
;; ============================================================

(define (key-event->description ke)
  (define parts '())
  (when (key-event-ctrl? ke)  (set! parts (cons "C-" parts)))
  (when (key-event-meta? ke)  (set! parts (cons "M-" parts)))
  (when (key-event-shift? ke) (set! parts (cons "S-" parts)))
  (cond
    [(key-event-symbol ke)
     (string-append (string-join parts "") (symbol->string (key-event-symbol ke)))]
    [(key-event-char ke)
     (define ch (key-event-char ke))
     (string-append (string-join parts "")
                    (match ch
                      [#\space "SPC"] [#\newline "RET"] [#\tab "TAB"]
                      [#\rubout "DEL"]
                      [_ (if (< (char->integer ch) 32)
                             (format "C-~a"
                               (integer->char (+ (char->integer ch) 64)))
                             (if (and (key-event-ctrl? ke) (char-alphabetic? ch))
                                 (string (char-downcase ch))
                                 (string ch)))]))]
    [else "???"]))
