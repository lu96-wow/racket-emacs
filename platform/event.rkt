#lang racket

;; platform/event.rkt — Minimal key event reader
;;
;; Reads raw bytes from stdin, produces key-event structs.
;; Depends on kernel/key-event for the abstract key event type.
;; Handles: ASCII, basic CSI sequences (arrows, home/end, delete),
;; and common 8-bit meta.

(require "../kernel/key-event/key-event.rkt")

(provide
 read-key-event!
 read-byte!
 )

;; ============================================================
;; Constants
;; ============================================================

(define ESC 27)
(define CSI 91)   ;; '['
(define SS3 79)   ;; 'O'

;; ============================================================
;; Byte I/O
;; ============================================================

(define (read-byte!)
  (define evt (sync (read-bytes-evt 1 (current-input-port))))
  (if (bytes? evt)
      (bytes-ref evt 0)
      (error 'read-byte! "unexpected event: ~a" evt)))

(define (read-byte!/timeout sec)
  (define evt (sync/timeout sec (read-bytes-evt 1 (current-input-port))))
  (and (bytes? evt) (= (bytes-length evt) 1) (bytes-ref evt 0)))

;; ============================================================
;; CSI sequence → symbol
;; ============================================================

;; CSI final byte → symbol (simple final bytes only: A B C D H F)
(define csi-symbol
  (hasheq
   (char->integer #\A) 'up
   (char->integer #\B) 'down
   (char->integer #\C) 'right
   (char->integer #\D) 'left
   (char->integer #\H) 'home
   (char->integer #\F) 'end))

;; SS3 final byte → symbol (O A, O B, ...)
(define ss3-symbol
  (hasheq
   (char->integer #\A) 'up
   (char->integer #\B) 'down
   (char->integer #\C) 'right
   (char->integer #\D) 'left
   (char->integer #\H) 'home
   (char->integer #\F) 'end))

;; ============================================================
;; Read one key event
;; ============================================================

(define (read-key-event!)
  (define b (read-byte!))
  (cond
    ;; Enter / Return
    [(or (= b 13) (= b 10))
     (key-event #\newline #f #f #f 'return)]

    ;; Tab
    [(= b 9)
     (key-event #\tab #f #f #f 'tab)]

    ;; Backspace (DEL=127 or BS=8)
    [(or (= b 127) (= b 8))
     (key-event #f #f #f #f 'backspace)]

    ;; Escape
    [(= b ESC)
     (parse-escape)]

    ;; Control characters: C-@ (0) through C-_ (31), except TAB/LF/CR/ESC
    [(<= 0 b 31)
     (key-event (integer->char (+ b 64)) #t #f #f #f)]

    ;; Printable ASCII
    [(<= 32 b 126)
     (key-event (integer->char b) #f #f #f #f)]

    ;; 8-bit: high-byte set → meta + low 7 bits
    [(<= 128 b 255)
     (define lo (- b 128))
     (cond [(= lo (char->integer #\[))
            ;; Meta-[ → treat as ESC [
            (define sym (parse-csi-sequence read-byte!/timeout CSI #f))
            (if sym
                (key-event #f #f #t #f sym)
                (key-event #f #f #t #f 'escape))]
           [(= lo (char->integer #\O))
            ;; Meta-O → treat as ESC O
            (define sym (parse-csi-sequence read-byte!/timeout SS3 #t))
            (if sym
                (key-event #f #f #t #f sym)
                (key-event #f #f #t #f 'escape))]
           [(<= 32 lo 126)
            (key-event (integer->char lo) #f #t #f #f)]
           [else
            (key-event (integer->char lo) #t #t #f #f)])]

    ;; UTF-8 multi-byte
    [(<= 194 b 244)
     (define rest (read-utf8-rest b))
     (define full (bytes-append (bytes b) rest))
     (define str (bytes->string/utf-8 full))
     (if (and (= (string-length str) 1)
              (>= (char->integer (string-ref str 0)) 32))
         (key-event (string-ref str 0) #f #f #f #f)
         (key-event #f #f #f #f 'unknown))]

    [else
     (key-event #f #f #f #f 'unknown)]))

;; ============================================================
;; ESC sequence parsing
;; ============================================================

(define (parse-escape)
  (define b2 (read-byte!/timeout 0.05))
  (cond
    [(not b2)
     ;; Bare ESC
     (key-event #f #f #f #f 'escape)]

    [(= b2 CSI)
     ;; ESC [
     (define sym (parse-csi-sequence read-byte!/timeout CSI #f))
     (if sym
         (key-event #f #f #f #f sym)
         (key-event #f #f #f #f 'escape))]

    [(= b2 SS3)
     ;; ESC O
     (define sym (parse-csi-sequence read-byte!/timeout SS3 #t))
     (if sym
         (key-event #f #f #f #f sym)
         (key-event #f #f #f #f 'escape))]

    [(<= 32 b2 126)
     ;; ESC <printable> → Meta-<char>
     (key-event (integer->char b2) #f #t #f #f)]

    [else
     (key-event #f #f #f #f 'escape)]))

(define (parse-csi-sequence read-timeout opener ss3?)
  (define b (read-timeout 0.05))
  (and b
       (cond
         ;; Numbered: \e[2~ (insert), \e[3~ (delete), \e[5~ (PgUp), \e[6~ (PgDn)
         [(and (not ss3?) (<= 48 b 57))
          (parse-csi-numbered read-timeout b)]
         ;; SS3 lookup
         [ss3?
          (hash-ref ss3-symbol b (λ () #f))]
         ;; Final byte (0x40-0x7F) — simple CSI lookup
         [(<= #x40 b #x7E)
          (hash-ref csi-symbol b (λ () #f))]
         ;; Unrecognized CSI (mouse report, etc.) → drain the rest
         [else
          (drain-csi-sequence read-timeout b)
          #f])))

(define (parse-csi-numbered read-timeout b1)
  ;; Read digits until a tilde (~) or another final byte
  (let loop ([n (- b1 48)]  ; first digit
             [count 0])
    (if (> count 10) #f
        (let ([b (read-timeout 0.05)])
          (cond
            [(not b) #f]
            [(= b 126)  ; ~  final
             (match n
               [2 'insert] [3 'delete]
               [5 'prior] [6 'next]
               [1 'home] [4 'end]  ; some terminals
               [_ #f])]
            [(<= 48 b 57)
             (loop (+ (* n 10) (- b 48)) (add1 count))]
            ;; Not a digit, not '~' — drain rest of CSI
            [else
             (drain-csi-sequence read-timeout b)
             #f])))))

;; ============================================================
;; CSI drain — consume bytes until a final byte (0x40-0x7E)
;; ============================================================
;; Called when we encounter an unrecognized CSI sequence
;; (mouse reports, unsupported escape codes, etc.).
;; Reads and discards bytes until the sequence terminates.

(define (drain-csi-sequence read-timeout first-unexpected)
  ;; If the first unexpected byte is already a final byte, we are done.
  (when (and first-unexpected (< first-unexpected #x40))
    (let drain ()
      (define b (read-timeout 0.05))
      (when (and b (< b #x40))
        (drain)))))

;; ============================================================
;; UTF-8 rest
;; ============================================================

(define (read-utf8-rest first)
  (define len
    (cond [(<= 194 first 223) 1]
          [(<= 224 first 239) 2]
          [(<= 240 first 244) 3]
          [else 0]))
  (let loop ([i 0] [acc (bytes)])
    (if (>= i len)
        acc
        (let ([b (read-byte!/timeout 0.1)])
          (if b
              (loop (add1 i) (bytes-append acc (bytes b)))
              acc)))))
