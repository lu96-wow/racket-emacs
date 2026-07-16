#lang racket

;; platform/event.rkt — Key + mouse event reader
;;
;; Reads raw bytes from stdin, produces key-event or mouse-event structs.
;; Mouse events are a separate type — they NEVER go through key processing.
;; Handles: ASCII, CSI sequences, UTF-8, 8-bit meta, SGR mouse reports.

(require "../kernel/key-event/key-event.rkt")

(provide
 read-key-event!
 read-byte!
 ;; constants for other modules
 TAB LF CR ESC SPACE BACKSPACE DELETE ESCDELAY)

(define TAB 9)           (define LF 10)           (define CR 13)
(define ESC 27)          (define SPACE 32)
(define BACKSPACE 8)     (define DELETE 127)
(define CSI-OPEN 91)     (define CSI-SS3 79)
(define CSI-FINAL-START 64)  (define CSI-FINAL-END 126)
(define UTF8-2BYTE-START 194) (define UTF8-2BYTE-END 223)
(define UTF8-3BYTE-START 224) (define UTF8-3BYTE-END 239)
(define UTF8-4BYTE-START 240) (define UTF8-4BYTE-END 244)
(define ASCII-PRINTABLE-START 32) (define ASCII-PRINTABLE-END 126)

(define ESCDELAY 0.1)

;; ============================================================
;; Byte I/O
;; ============================================================

(define (make-stdin-evt) (read-bytes-evt 1 (current-input-port)))

(define (read-byte!)
  (define evt (sync (make-stdin-evt)))
  (if (bytes? evt)
      (bytes-ref evt 0)
      (error 'read-byte! "unexpected event: ~a" evt)))

(define (read-byte!/timeout sec)
  (define evt (sync/timeout sec (make-stdin-evt)))
  (and (bytes? evt) (= (bytes-length evt) 1) (bytes-ref evt 0)))

;; ============================================================
;; Read one input event (key-event or mouse-event)
;; ============================================================

(define (read-key-event!)
  (define b (read-byte!))
  (cond
    [(<= 0 b 31)
     (cond [(= b TAB)   (key-event #\tab #f #f #f 'tab)]
           [(= b LF)    (key-event #\return #f #f #f 'return)]
           [(= b CR)    (key-event #\return #f #f #f 'return)]
           [(= b BACKSPACE) (key-event #f #f #f #f 'backspace)]
           [(= b ESC)   (parse-escape)]
           [else (key-event (integer->char (+ b 64)) #t #f #f #f)])]
    [(= b DELETE) (key-event #f #f #f #f 'backspace)]
    [(<= ASCII-PRINTABLE-START b ASCII-PRINTABLE-END)
     (key-event (integer->char b) #f #f #f #f)]
    [(<= UTF8-2BYTE-START b UTF8-4BYTE-END)
     (define rest (read-utf8-rest b))
     (define full (bytes-append (bytes b) rest))
     (define str (bytes->string/utf-8 full))
     (if (and (= (string-length str) 1)
              (>= (char->integer (string-ref str 0)) 32))
         (key-event (string-ref str 0) #f #f #f #f)
         (key-event #f #f #f #f 'unknown))]
    [(<= 128 b 255)
     (define lo (- b 128))
     (cond [(= lo (char->integer #\[))
            (parse-escape)]  ;; Meta-[ → same as ESC [
           [(= lo (char->integer #\O))
            (parse-escape)]  ;; Meta-O → same as ESC O
           [(<= 32 lo 126)
            (key-event (integer->char lo) #f #t #f #f)]
           [else
            (key-event (integer->char lo) #t #t #f #f)])]
    [else (key-event #f #f #f #f 'unknown)]))

;; ============================================================
;; ESC sequence parsing
;; ============================================================

(define (parse-escape)
  (define b2 (read-byte!/timeout ESCDELAY))
  (cond
    [(not b2) (key-event #f #f #f #f 'escape)]
    [(= b2 CSI-OPEN)
     ;; ESC [ — read the CSI sequence bytes (including ESC + [)
     (define seq-bytes (read-csi-seq b2))
     ;; Check for SGR mouse: \e[<...M or \e[<...m
     (if (and (>= (bytes-length seq-bytes) 4)
              (= (bytes-ref seq-bytes 2) #x3C)  ; '<'
              (let ([final (bytes-ref seq-bytes (sub1 (bytes-length seq-bytes)))])
                (or (= final (char->integer #\M))
                    (= final (char->integer #\m)))))
         (decode-sgr-mouse seq-bytes)
         ;; Not mouse — try CSI lookup
         (parse-csi-seq-result seq-bytes))]
    [(= b2 CSI-SS3)
     (define b3 (read-byte!/timeout ESCDELAY))
     (if b3
         (let ([sym (ss3-lookup b3)])
           (if sym
               (key-event #f #f #f #f sym)
               (key-event #f #f #f #f 'escape)))
         (key-event #f #f #f #f 'escape))]
    [(<= ASCII-PRINTABLE-START b2 ASCII-PRINTABLE-END)
     ;; ESC <printable> → Meta-<char>
     (key-event (integer->char b2) #f #t #f #f)]
    [else (key-event #f #f #f #f 'escape)]))

;; ============================================================
;; CSI helpers
;; ============================================================

(define (read-csi-seq b2 [b3 #f])
  ;; Read a complete CSI sequence.  b2 is the open-bracket byte.
  ;; Returns bytes including ESC and [.
  (let loop ([acc (if b3 (list ESC b2 b3) (list ESC b2))])
    (define b (read-byte!/timeout ESCDELAY))
    (cond [(not b) (list->bytes acc)]
          [(<= CSI-FINAL-START b CSI-FINAL-END) (list->bytes (append acc (list b)))]
          [else (loop (append acc (list b)))])))

(define (parse-csi-seq-result seq-bytes)
  ;; Try to match a simple CSI final byte against known symbols.
  ;; seq-bytes includes ESC + [ + parameters + final.
  (define len (bytes-length seq-bytes))
  (if (>= len 3)
      (let ([final (bytes-ref seq-bytes (sub1 len))])
        (let ([sym (hash-ref csi-symbol-table final (λ () #f))])
          (if sym
              (key-event #f #f #f #f sym)
              (key-event #f #f #f #f 'escape))))
      (key-event #f #f #f #f 'escape)))

(define csi-symbol-table
  (let ([h (make-hasheq)])
    (for ([final (in-list (list (char->integer #\A) (char->integer #\B)
                                (char->integer #\C) (char->integer #\D)
                                (char->integer #\H) (char->integer #\F)))]
          [sym   (in-list '(up down right left home end))])
      (hash-set! h final sym))
    h))

(define (ss3-lookup b)
  (hash-ref (hasheq (char->integer #\A) 'up
                    (char->integer #\B) 'down
                    (char->integer #\C) 'right
                    (char->integer #\D) 'left
                    (char->integer #\H) 'home
                    (char->integer #\F) 'end)
            b (λ () #f)))

;; ============================================================
;; SGR mouse — \e[<button;x;yM  or  \e[<button;x;ym
;; ============================================================

(define (decode-sgr-mouse seq)
  (define slen (bytes-length seq))
  (define final-b (bytes-ref seq (sub1 slen)))
  ;; seq is: ESC [ < params M/m, params start at byte index 3
  (define-values (cb _1)    (parse-mouse-field seq 3 (char->integer #\;)))
  (define-values (x  off2)  (parse-mouse-field seq (+ 3 _1 1) (char->integer #\;)))
  (define-values (y  _off3) (parse-mouse-field seq (+ 3 _1 1 off2 1) final-b))
  (define scroll? (>= cb 64))
  (define raw-btn (if scroll? cb (bitwise-and cb #b11)))
  (define btn (case raw-btn
                [(0) 'left] [(1) 'middle] [(2) 'right]
                [(64) 'wheel-up] [(65) 'wheel-down]
                [else 'unknown]))
  (define action (cond [scroll? (if (= (bitwise-and cb 1) 0) 'scroll-up 'scroll-down)]
                       [(= final-b (char->integer #\m)) 'release]
                       [else 'press]))
  (define mods (arithmetic-shift cb -2))
  (mouse-event btn (sub1 x) (sub1 y) action
               (bitwise-bit-set? mods 0)
               (bitwise-bit-set? mods 1)
               (bitwise-bit-set? mods 2)))

(define (parse-mouse-field seq start stop-byte)
  (let loop ([i start] [n 0])
    (if (>= i (bytes-length seq)) (values n (- i start))
        (let ([b (bytes-ref seq i)])
          (cond [(<= 48 b 57) (loop (add1 i) (+ (* n 10) (- b 48)))]
                [(= b stop-byte) (values n (- i start))]
                [else (loop (add1 i) n)])))))

;; ============================================================
;; UTF-8
;; ============================================================

(define (read-utf8-rest first)
  (define len (cond [(<= UTF8-2BYTE-START first UTF8-2BYTE-END) 1]
                    [(<= UTF8-3BYTE-START first UTF8-3BYTE-END) 2]
                    [(<= UTF8-4BYTE-START first UTF8-4BYTE-END) 3]
                    [else 0]))
  (let loop ([n 0] [acc (bytes)])
    (if (>= n len) acc
        (let ([b (read-byte!/timeout 0.1)])
          (if b (loop (add1 n) (bytes-append acc (bytes b))) acc)))))
