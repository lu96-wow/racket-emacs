#lang racket

;; base/keybind.rkt — String→key-event DSL

(require "../kernel/keymap.rkt")

(provide bind-key string->key-event string->key-sequence)

(define (bind-key km spec cmd) (define-key km (string->key-sequence spec) cmd))

(define (string->key-sequence str) (map string->key-event (string-split str)))

(define (string->key-event s)
  (define ctrl? #f) (define meta? #f) (define shift? #f) (define name s)
  (let parse-loop ()
    (cond [(str-prefix? name "C-M-S-") (set! ctrl? #t) (set! meta? #t) (set! shift? #t) (set! name (substring name 6)) (parse-loop)]
          [(str-prefix? name "C-M-")   (set! ctrl? #t) (set! meta? #t) (set! name (substring name 4)) (parse-loop)]
          [(str-prefix? name "C-S-")   (set! ctrl? #t) (set! shift? #t) (set! name (substring name 4)) (parse-loop)]
          [(str-prefix? name "M-S-")   (set! meta? #t) (set! shift? #t) (set! name (substring name 4)) (parse-loop)]
          [(str-prefix? name "C-")     (set! ctrl? #t) (set! name (substring name 2)) (parse-loop)]
          [(str-prefix? name "M-")     (set! meta? #t) (set! name (substring name 2)) (parse-loop)]
          [(str-prefix? name "S-")     (set! shift? #t) (set! name (substring name 2)) (parse-loop)]
          [else (void)]))
  (cond [(= (string-length name) 1) (key-event (string-ref name 0) ctrl? meta? shift? #f)]
        [(string-ci=? name "RET")  (key-event #\return ctrl? meta? shift? 'return)]
        [(string-ci=? name "TAB")  (key-event #\tab ctrl? meta? shift? 'tab)]
        [(string-ci=? name "SPC")  (key-event #\space ctrl? meta? shift? #f)]
        [(string-ci=? name "DEL")  (key-event #f ctrl? meta? shift? 'backspace)]
        [(string-ci=? name "DELETE") (key-event #f ctrl? meta? shift? 'delete)]
        [(string-ci=? name "ESC")  (key-event #f ctrl? meta? shift? 'escape)]
        [(string-ci=? name "up") (key-event #f ctrl? meta? shift? 'up)]
        [(string-ci=? name "down") (key-event #f ctrl? meta? shift? 'down)]
        [(string-ci=? name "left") (key-event #f ctrl? meta? shift? 'left)]
        [(string-ci=? name "right") (key-event #f ctrl? meta? shift? 'right)]
        [(string-ci=? name "home") (key-event #f ctrl? meta? shift? 'home)]
        [(string-ci=? name "end")  (key-event #f ctrl? meta? shift? 'end)]
        [(string-ci=? name "prior") (key-event #f ctrl? meta? shift? 'prior)]
        [(string-ci=? name "next")  (key-event #f ctrl? meta? shift? 'next)]
        [(str-prefix? name "f") (define n (string->number (substring name 1)))
         (key-event #f ctrl? meta? shift? (string->symbol (format "f~a" n)))]
        [else (error 'string->key-event "unknown key: ~a" s)]))

(define (str-prefix? str prefix) (and (>= (string-length str) (string-length prefix))
                                      (string=? (substring str 0 (string-length prefix)) prefix)))
