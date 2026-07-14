#lang racket

;; platform/event.rkt — Raw byte reading → key-event decoding

(require "../kernel/key-event.rkt")

(provide
 TAB LF CR ESC SPACE BACKSPACE DELETE
 CSI-OPEN CSI-SS3 CSI-FINAL-START CSI-FINAL-END
 UTF8-2BYTE-START UTF8-2BYTE-END UTF8-3BYTE-START UTF8-3BYTE-END
 UTF8-4BYTE-START UTF8-4BYTE-END
 ASCII-PRINTABLE-START ASCII-PRINTABLE-END

 mouse-event? mouse-event
 mouse-event-button mouse-event-x mouse-event-y
 mouse-event-action mouse-event-shift? mouse-event-alt? mouse-event-ctrl?

 read-key-event! read-key-event/timeout!
 read-byte! read-byte!/timeout
 ESCDELAY CSI-MAX-BYTES
 csi-bytes->key-sequence
 decode-sgr-mouse
 init-input-decode-map!)

(define TAB 9)           (define LF 10)           (define CR 13)
(define ESC 27)          (define SPACE 32)
(define BACKSPACE 8)     (define DELETE 127)
(define CSI-OPEN 91)            (define CSI-SS3 79)
(define CSI-FINAL-START 64)     (define CSI-FINAL-END 126)
(define UTF8-2BYTE-START 194)   (define UTF8-2BYTE-END 223)
(define UTF8-3BYTE-START 224)   (define UTF8-3BYTE-END 239)
(define UTF8-4BYTE-START 240)   (define UTF8-4BYTE-END 244)
(define ASCII-PRINTABLE-START 32) (define ASCII-PRINTABLE-END 126)

(struct mouse-event (button x y action shift? alt? ctrl?) #:transparent)

(define ESCDELAY 0.1)
(define CSI-MAX-BYTES 32)
(define UTF8-CONTINUATION-TIMEOUT 0.1)

(define (make-stdin-evt) (read-bytes-evt 1 (current-input-port)))

(define (read-byte!)
  (define evt (sync (make-stdin-evt)))
  (if (bytes? evt) (bytes-ref evt 0)
      (error 'read-byte! "unexpected event: ~a" evt)))

(define (read-byte!/timeout timeout-sec)
  (define evt (sync/timeout timeout-sec (make-stdin-evt)))
  (and (bytes? evt) (= (bytes-length evt) 1) (bytes-ref evt 0)))

(define (read-csi-seq b2 [b3 #f])
  (let loop ([acc (if b3 (list ESC b2 b3) (list ESC b2))] [left CSI-MAX-BYTES])
    (if (zero? left) (list->bytes acc)
        (let ([b (read-byte!/timeout ESCDELAY)])
          (cond [(not b) (list->bytes acc)]
                [(<= CSI-FINAL-START b CSI-FINAL-END) (list->bytes (append acc (list b)))]
                [else (loop (append acc (list b)) (sub1 left))])))))

(define (read-utf8-rest first)
  (define len (cond [(<= UTF8-2BYTE-START first UTF8-2BYTE-END) 1]
                    [(<= UTF8-3BYTE-START first UTF8-3BYTE-END) 2]
                    [(<= UTF8-4BYTE-START first UTF8-4BYTE-END) 3]
                    [else 0]))
  (let loop ([n 0] [acc (bytes)])
    (if (>= n len) acc
        (let ([b (read-byte!/timeout UTF8-CONTINUATION-TIMEOUT)])
          (if b (loop (add1 n) (bytes-append acc (bytes b))) acc)))))

(define (csi-bytes->key-sequence bstr)
  (for/list ([b (in-bytes bstr)]) (key-event (integer->char b) #f #f #f #f)))

(define (read-key-event! [input-decode-map #f] [lookup-fn #f])
  (read-key-event* read-byte! input-decode-map lookup-fn))

(define (read-key-event/timeout! timeout-sec [input-decode-map #f] [lookup-fn #f])
  (define b (read-byte!/timeout timeout-sec))
  (and b (read-key-event* (λ () b) input-decode-map lookup-fn)))

(define (read-key-event* get-first-byte input-decode-map lookup-fn)
  (define b (get-first-byte))
  (cond
    [(<= 0 b 31)
     (cond [(= b TAB)   (key-event #\tab #f #f #f 'tab)]
           [(= b LF)    (key-event #\return #f #f #f 'return)]
           [(= b CR)    (key-event #\return #f #f #f 'return)]
           [(= b BACKSPACE) (key-event #f #f #f #f 'backspace)]
           [(= b ESC)   (parse-escape-via-keymap input-decode-map lookup-fn)]
           [else (key-event (integer->char (+ b 64)) #t #f #f #f)])]
    [(= b DELETE) (key-event #f #f #f #f 'backspace)]
    [(<= ASCII-PRINTABLE-START b ASCII-PRINTABLE-END)
     (key-event (integer->char b) #f #f #f #f)]
    [(<= UTF8-2BYTE-START b UTF8-4BYTE-END)
     (define rest (read-utf8-rest b))
     (define str (bytes->string/utf-8 (bytes-append (bytes b) rest)))
     (if (= (string-length str) 1)
         (key-event (string-ref str 0) #f #f #f #f)
         (key-event #f #f #f #f 'unknown))]
    [(<= 128 b 255)
     (define lo (- b 128))
     (cond [(and input-decode-map lookup-fn (= lo (char->integer #\[)))
            (parse-escape-via-keymap input-decode-map lookup-fn)]
           [(and input-decode-map lookup-fn (= lo (char->integer #\O)))
            (parse-escape-via-keymap input-decode-map lookup-fn)]
           [else (key-event (integer->char lo) #f #t #f #f)])]
    [else (key-event #f #f #f #f 'unknown)]))

(define (parse-escape-via-keymap decode-map lookup-fn)
  (define b2 (read-byte!/timeout ESCDELAY))
  (cond
    [(not b2) (key-event #f #f #t #f 'escape)]
    [(= b2 CSI-OPEN)
     (define seq-bytes (read-csi-seq b2))
     (cond [(and (>= (bytes-length seq-bytes) 3)
                 (= (bytes-ref seq-bytes 2) 60)
                 (let ([final (bytes-ref seq-bytes (sub1 (bytes-length seq-bytes)))])
                   (or (= final 77) (= final 109))))
            (decode-sgr-mouse seq-bytes)]
           [else
            (if (and decode-map lookup-fn)
                (let* ([ke-seq (csi-bytes->key-sequence seq-bytes)]
                       [translated (lookup-fn decode-map ke-seq)])
                  (if (key-event? translated) translated
                      (key-event #f #f #t #f 'escape)))
                (key-event #f #f #t #f 'escape))])]
    [(= b2 CSI-SS3)
     (define b3 (read-byte!/timeout ESCDELAY))
     (if (and decode-map lookup-fn)
         (let* ([ke-seq (list (key-event (integer->char ESC) #f #f #f #f)
                              (key-event #\O #f #f #f #f)
                              (key-event (integer->char (if b3 b3 0)) #f #f #f #f))]
                [translated (lookup-fn decode-map ke-seq)])
           (if (key-event? translated) translated
               (key-event #f #f #t #f 'escape)))
         (key-event #f #f #t #f 'escape))]
    [(<= ASCII-PRINTABLE-START b2 ASCII-PRINTABLE-END)
     (key-event (integer->char b2) #f #t #f #f)]
    [else (key-event #f #f #t #f 'escape)]))

(define (decode-sgr-mouse seq)
  (define slen (bytes-length seq))
  (define final-b (bytes-ref seq (sub1 slen)))
  (define-values (cb _1)    (parse-mouse-field seq 3 #\;))
  (define-values (x  off2)  (parse-mouse-field seq (+ 3 _1 1) #\;))
  (define-values (y  _off3) (parse-mouse-field seq (+ 3 _1 1 off2 1) (integer->char final-b)))
  (define mods (arithmetic-shift cb -2))
  (define scroll? (bitwise-bit-set? cb 6))
  (define raw-cb (if scroll? (bitwise-and cb #x43) (bitwise-and cb #b11)))
  (define btn (case raw-cb [(0) 'left] [(1) 'middle] [(2) 'right]
                [(64) 'wheel-up] [(65) 'wheel-down] [else 'unknown]))
  (define action (cond [scroll? (if (= (bitwise-and cb 1) 0) 'scroll-up 'scroll-down)]
                       [(= final-b 109) 'release] [else 'press]))
  (mouse-event btn (sub1 x) (sub1 y) action
               (bitwise-bit-set? mods 0) (bitwise-bit-set? mods 1) (bitwise-bit-set? mods 2)))

(define (parse-mouse-field seq start stop-char)
  (let loop ([i start] [n 0])
    (if (>= i (bytes-length seq)) (values n (- i start))
        (let ([b (bytes-ref seq i)])
          (cond [(<= 48 b 57) (loop (add1 i) (+ (* n 10) (- b 48)))]
                [(= b (char->integer stop-char)) (values n (- i start))]
                [else (loop (add1 i) n)])))))

(define (mod->flags mod)
  (case mod [(2) (values #f #f #t)] [(3) (values #f #t #f)] [(4) (values #f #t #t)]
    [(5) (values #t #f #f)] [(6) (values #t #f #t)] [(7) (values #t #t #f)]
    [(8) (values #t #t #t)] [else (values #f #f #f)]))

(define (init-input-decode-map! km define-key!)
  (define (bind str ke)
    (define seq (for/list ([c (in-string str)]) (key-event c #f #f #f #f)))
    (define-key! km seq ke))
  (bind "\e[A" (key-event #f #f #f #f 'up))
  (bind "\e[B" (key-event #f #f #f #f 'down))
  (bind "\e[C" (key-event #f #f #f #f 'right))
  (bind "\e[D" (key-event #f #f #f #f 'left))
  (bind "\e[H" (key-event #f #f #f #f 'home))
  (bind "\e[F" (key-event #f #f #f #f 'end))
  (bind "\e[Z" (key-event #f #f #t #f 'tab))
  (bind "\e[2~"  (key-event #f #f #f #f 'insert))
  (bind "\e[3~"  (key-event #f #f #f #f 'delete))
  (bind "\e[5~"  (key-event #f #f #f #f 'prior))
  (bind "\e[6~"  (key-event #f #f #f #f 'next))
  (bind "\e[11~" (key-event #f #f #f #f 'f1))
  (bind "\e[12~" (key-event #f #f #f #f 'f2))
  (bind "\e[13~" (key-event #f #f #f #f 'f3))
  (bind "\e[14~" (key-event #f #f #f #f 'f4))
  (bind "\e[15~" (key-event #f #f #f #f 'f5))
  (bind "\e[17~" (key-event #f #f #f #f 'f6))
  (bind "\e[18~" (key-event #f #f #f #f 'f7))
  (bind "\e[19~" (key-event #f #f #f #f 'f8))
  (bind "\e[20~" (key-event #f #f #f #f 'f9))
  (bind "\e[21~" (key-event #f #f #f #f 'f10))
  (bind "\e[23~" (key-event #f #f #f #f 'f11))
  (bind "\e[24~" (key-event #f #f #f #f 'f12))
  (bind "\e[29~" (key-event #f #f #f #f 'menu))
  (bind "\eOA" (key-event #f #f #f #f 'up))
  (bind "\eOB" (key-event #f #f #f #f 'down))
  (bind "\eOC" (key-event #f #f #f #f 'right))
  (bind "\eOD" (key-event #f #f #f #f 'left))
  (bind "\eOF" (key-event #f #f #f #f 'end))
  (bind "\eOH" (key-event #f #f #f #f 'home))
  (bind "\eOP" (key-event #f #f #f #f 'f1))
  (bind "\eOQ" (key-event #f #f #f #f 'f2))
  (bind "\eOR" (key-event #f #f #f #f 'f3))
  (bind "\eOS" (key-event #f #f #f #f 'f4))
  (for ([final (in-string "ABCDHF")] [sym (in-list '(up down right left end home))])
    (for ([mod (in-list '(2 3 4 5 6 7 8))])
      (define-values (c? m? s?) (mod->flags mod))
      (bind (format "\e[1;~a~a" mod (string final)) (key-event #f c? m? s? sym))))
  (for ([n (in-list '(2 3 5 6))] [sym (in-list '(insert delete prior next))])
    (for ([mod (in-list '(2 3 4 5 6 7 8))])
      (define-values (c? m? s?) (mod->flags mod))
      (bind (format "\e[~a;~a~~" n mod) (key-event #f c? m? s? sym))))
  km)
