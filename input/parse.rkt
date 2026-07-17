#lang racket

;; input/parse.rkt — Raw stdin bytes → key event
;;
;; Reads raw bytes from stdin (raw mode, VMIN=0 VTIME=1).
;; Parses: ASCII, CSI escape sequences, SS3, UTF-8, SGR mouse.
;; Reference: racket-emacs/platform/event.rkt
;;
;; Dependencies: input/key, platform/termios

(require "key.rkt"
         "../platform/termios.rkt")

(provide
 read-key)

;; ============================================================
;; Constants
;; ============================================================

(define ESC 27)          (define CSI-OPEN 91)      (define SS3 79)
(define TAB 9)           (define LF 10)             (define CR 13)
(define SPACE 32)        (define BACKSPACE 8)       (define DELETE 127)
(define CSI-FINAL-LO 64) (define CSI-FINAL-HI 126)
(define CSI-PARAM-SEP 59)                           ;; semicolon ;
(define ASCII-DIGIT-LO 48) (define ASCII-DIGIT-HI 57)
(define UTF8-2B-LO 194)  (define UTF8-2B-HI 223)
(define UTF8-3B-LO 224)  (define UTF8-3B-HI 239)
(define UTF8-4B-LO 240)  (define UTF8-4B-HI 244)
(define ESCDELAY 0.1)    ;; 100ms for escape sequence detection
(define UTF8-TIMEOUT 0.02) ;; 20ms for UTF-8 continuation bytes

;; SGR mouse
(define MOUSE-EVENT     (char->integer #\M))
(define MOUSE-RELEASE   (char->integer #\m))
(define MOUSE-MASK      3)
(define MOUSE-MOVE-FLAG 32)
(define MOUSE-SCROLL-LO 64)
(define MOUSE-SCROLL-HI 67)

;; ============================================================
;; I/O — event-based (sync + make-stdin-evt)
;; ============================================================

(define (make-stdin-evt) (read-bytes-evt 1 (current-input-port)))

(define (read-byte)
  ;; sync on both stdin and resize channel
  (define evt (sync (make-stdin-evt) resize-channel))
  (if (eq? evt 'resize)
      'resize
      (and (bytes? evt) (bytes-ref evt 0))))

(define (read-byte/timeout sec)
  (define evt (sync/timeout sec (make-stdin-evt) resize-channel))
  (cond
    [(eq? evt 'resize) 'resize]
    [(bytes? evt) (and (= (bytes-length evt) 1) (bytes-ref evt 0))]
    [else #f]))

(define (read-bytes-n/timeout n sec)
  (let loop ([i 0] [acc (bytes)])
    (if (>= i n) acc
        (let ([b (read-byte/timeout sec)])
          (if b (loop (+ i 1) (bytes-append acc (bytes b))) acc)))))

;; ============================================================
;; UTF-8
;; ============================================================

(define (utf8-multi-start? b) (<= UTF8-2B-LO b UTF8-4B-HI))

(define (utf8-len b)
  (cond [(<= UTF8-2B-LO b UTF8-2B-HI) 2]
        [(<= UTF8-3B-LO b UTF8-3B-HI) 3]
        [(<= UTF8-4B-LO b UTF8-4B-HI) 4]
        [else 1]))

;; ============================================================
;; CSI helpers
;; ============================================================

(define (csi-final? b) (<= CSI-FINAL-LO b CSI-FINAL-HI))

(define (csi-digit? b) (<= ASCII-DIGIT-LO b ASCII-DIGIT-HI))

(define (read-csi-seq b2)
  (let loop ([acc (list ESC b2)])
    (define b (read-byte/timeout ESCDELAY))
    (cond [(or (eq? b 'resize) (not b)) (list->bytes acc)]
          [(csi-final? b) (list->bytes (append acc (list b)))]
          [else (loop (append acc (list b)))])))

(define (parse-csi-params seq)
  ;; Parse semicolon-separated integer parameters from CSI sequence.
  ;; Returns (values params final-byte).
  (define len (bytes-length seq))
  (let loop ([i 2] [cur 0] [ps '()])
    (if (>= i len) (values '() 0)
        (let ([b (bytes-ref seq i)])
          (cond [(csi-digit? b)
                 (loop (+ i 1) (+ (* cur 10) (- b ASCII-DIGIT-LO)) ps)]
                [(= b CSI-PARAM-SEP)
                 (loop (+ i 1) 0 (append ps (list cur)))]
                [(csi-final? b)
                 (if (and (= i 2) (zero? cur) (null? ps))
                     (values '() b)
                     (values (append ps (list cur)) b))]
                [else (loop (+ i 1) cur ps)])))))

(define (csi-to-key params final)
  (case final
    [(65) (key-sym 'up)]
    [(66) (key-sym 'down)]
    [(67) (key-sym 'right)]
    [(68) (key-sym 'left)]
    [(72) (key-sym 'home)]
    [(70) (key-sym 'end)]
    [else
     (and (= final 126)  ;; CSI ... ~  (Insert/Delete/PageUp/PageDown)
          (pair? params)
          (case (car params)
            [(2) (key-sym 'insert)]
            [(3) (key-sym 'delete)]
            [(5) (key-sym 'pageup)]
            [(6) (key-sym 'pagedown)]
            [else #f]))]))

;; ============================================================
;; SGR mouse — \e[<button;x;yM  or  \e[<button;x;ym
;; ============================================================

(define (sgr-mouse? params)
  ;; SGR mouse always starts with \e[<  (byte 60 = #\<)
  ;; params is the list of integers, first param is the button code
  (and (pair? params) (>= (car params) 0)))

;; Returns: (values 'mouse button x y action modifiers)
;; Called from the mouse detection path in read-key

(define (decode-sgr-mouse params final)
  (define type (car params))
  (define x    (if (>= (length params) 2) (cadr params) 0))
  (define y    (if (>= (length params) 3) (caddr params) 0))
  (define button-code (bitwise-and type MOUSE-MASK))
  (define move? (bitwise-bit-set? type 5))  ;; bit 5 = motion flag
  (define scroll? (<= MOUSE-SCROLL-LO type MOUSE-SCROLL-HI))
  (define release? (and (= final MOUSE-RELEASE) (not scroll?) (not move?)))
  (define mods (arithmetic-shift type -2))  ;; bits 2-7 = modifiers

  (define button
    (cond [scroll? (if (= type 64) 'wheel-up 'wheel-down)]
          [(= button-code 0) 'left]
          [(= button-code 1) 'middle]
          [(= button-code 2) 'right]
          [else 'unknown]))

  (define action
    (cond [scroll?  'scroll]
          [release? 'release]
          [move?    'move]
          [else     'press]))

  (values 'mouse button x y action mods))

;; ============================================================
;; SS3
;; ============================================================

(define (ss3-to-key b)
  (case b
    [(65) (key-sym 'up)]
    [(66) (key-sym 'down)]
    [(67) (key-sym 'right)]
    [(68) (key-sym 'left)]
    [(72) (key-sym 'home)]
    [(70) (key-sym 'end)]
    [else #f]))

;; ============================================================
;; read-key — main entry
;; ============================================================

(define (read-key)
  (define b (read-byte))
  (cond
    [(eq? b 'resize) (key-sym 'resize)]
    [(not b) (key-sym 'idle)]

    ;; ESC — could be bare, CSI, SS3, or mouse
    [(= b ESC)
     (define b2 (read-byte/timeout ESCDELAY))
     (cond [(not b2) (key-sym 'escape)]
           [(= b2 CSI-OPEN)
            (define seq (read-csi-seq b2))
            (parse-csi-dispatch seq)]
           [(= b2 SS3)
            (define b3 (read-byte/timeout ESCDELAY))
            (if b3 (or (ss3-to-key b3) (key-sym 'escape))
                (key-sym 'escape))]
           [(<= SPACE b2 DELETE)
            (key-sym 'escape)]  ;; Meta+char not yet supported
           [else (key-sym 'escape)])]

    ;; Control characters
    [(= b TAB)       (key-sym 'tab)]
    [(= b CR)        (key-sym 'return)]
    [(= b LF)        (key-sym 'return)]
    [(= b BACKSPACE) (key-sym 'backspace)]
    [(= b DELETE)    (key-sym 'backspace)]
    [(<= 0 b 31)     (key-ctrl (integer->char (+ b 96)))]

    ;; Printable ASCII
    [(<= SPACE b DELETE) (key-char (integer->char b))]

    ;; UTF-8 multi-byte
    [(utf8-multi-start? b)
     (define rest (read-bytes-n/timeout (sub1 (utf8-len b)) UTF8-TIMEOUT))
     (define full (bytes-append (bytes b) rest))
     (define str (bytes->string/utf-8 full))
     (if (= (string-length str) 1)
         (key-char (string-ref str 0))
         (key-sym 'unknown))]

    [else (key-sym 'unknown)]))

;; ============================================================
;; CSI dispatch — mouse vs keyboard
;; ============================================================

(define (parse-csi-dispatch seq)
  (define-values (params final) (parse-csi-params seq))
  (cond
    ;; SGR mouse: ESC [ < params M/m
    [(and (= (bytes-ref seq 2) #x3C)  ;; '<'
          (or (= final MOUSE-EVENT) (= final MOUSE-RELEASE))
          (>= (length params) 3))
     (decode-sgr-mouse params final)]

    ;; Regular CSI key
    [else
     (or (csi-to-key params final) (key-sym 'escape))]))
