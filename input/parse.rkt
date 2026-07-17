#lang racket

;; input/parse.rkt — Raw stdin bytes → key event
;;
;; Reads raw bytes from stdin (raw mode, VMIN=0 VTIME=1).
;; Parses escape sequences, UTF-8, control chars.
;; Reference: racket-tui/base/io/input.rkt (ESCDELAY, CSI parsing).
;;
;; Dependencies: input/key, platform/termios

(require "key.rkt"
         "../platform/termios.rkt")

(provide
 read-key        ;; → key-char? | key-ctrl? | key-sym?
 )

;; ============================================================
;; Constants
;; ============================================================

(define ESC 27)         (define CSI-OPEN 91)      (define SS3 79)
(define TAB 9)          (define LF 10)             (define CR 13)
(define SPACE 32)       (define BACKSPACE 8)       (define DELETE 127)
(define CSI-FINAL-LO 64) (define CSI-FINAL-HI 126)
(define UTF8-2B-LO 194) (define UTF8-2B-HI 223)
(define UTF8-3B-LO 224) (define UTF8-3B-HI 239)
(define UTF8-4B-LO 240) (define UTF8-4B-HI 244)
(define ESCDELAY 0.1)

;; ============================================================
;; I/O — event-based, same as original racket-emacs
;; ============================================================

(define (make-stdin-evt) (read-bytes-evt 1 (current-input-port)))

(define (read-stdin-byte)
  ;; Block until one byte arrives.
  (define evt (sync (make-stdin-evt)))
  (if (bytes? evt)
      (bytes-ref evt 0)
      eof))

(define (read-stdin-byte/timeout sec)
  ;; Read one byte with timeout in seconds.  Returns byte or #f.
  (define evt (sync/timeout sec (make-stdin-evt)))
  (and (bytes? evt) (= (bytes-length evt) 1) (bytes-ref evt 0)))

(define (read-n/timeout n)
  (let loop ([i 0] [acc (bytes)])
    (if (>= i n) acc
        (let ([b (read-stdin-byte/timeout)])
          (if b (loop (+ i 1) (bytes-append acc (bytes b))) acc)))))

;; ============================================================
;; UTF-8
;; ============================================================

(define (utf8-multi-start? b) (<= UTF8-2B-LO b UTF8-4B-HI))

(define (utf8-length b)
  (cond [(<= UTF8-2B-LO b UTF8-2B-HI) 2]
        [(<= UTF8-3B-LO b UTF8-3B-HI) 3]
        [(<= UTF8-4B-LO b UTF8-4B-HI) 4]
        [else 1]))

;; ============================================================
;; CSI helpers
;; ============================================================

(define (csi-final? b) (<= CSI-FINAL-LO b CSI-FINAL-HI))

(define (read-csi-seq b2)
  (let loop ([acc (list ESC b2)])
    (define b (read-stdin-byte/timeout ESCDELAY))
    (cond [(not b) (list->bytes acc)]
          [(csi-final? b) (list->bytes (append acc (list b)))]
          [else (loop (append acc (list b)))])))

(define (csi-to-key seq-bytes)
  ;; Simple CSI: expect ESC [ final-byte
  (when (>= (bytes-length seq-bytes) 3)
    (define final (bytes-ref seq-bytes (sub1 (bytes-length seq-bytes))))
    (case final
      [(65) (key-sym 'up)]
      [(66) (key-sym 'down)]
      [(67) (key-sym 'right)]
      [(68) (key-sym 'left)]
      [(72) (key-sym 'home)]
      [(70) (key-sym 'end)]
      [(51) ;; ESC [ 3 ~ → Delete if seq is ESC [ 3 ~
       (and (= (bytes-length seq-bytes) 5)
            (= (bytes-ref seq-bytes 4) 126)
            (key-sym 'delete))]
      [(53) ;; ESC [ 5 ~ → PageUp
       (and (= (bytes-length seq-bytes) 5)
            (= (bytes-ref seq-bytes 4) 126)
            (key-sym 'pageup))]
      [(54) ;; ESC [ 6 ~ → PageDown
       (and (= (bytes-length seq-bytes) 5)
            (= (bytes-ref seq-bytes 4) 126)
            (key-sym 'pagedown))]
      [else #f])))

(define (ss3-to-key b)
  (case b
    [(72) (key-sym 'home)]
    [(70) (key-sym 'end)]
    [else #f]))

;; ============================================================
;; read-key — main entry
;; ============================================================

(define (read-key)
  ;; Returns a key-char, key-ctrl, or key-sym.
  (define b (read-stdin-byte))
  (cond
    [(eof-object? b) (key-sym 'idle)]

    ;; ESC — could be bare or sequence
    [(= b ESC)
     (define b2 (read-stdin-byte/timeout ESCDELAY))
     (cond [(not b2) (key-sym 'escape)]
           [(= b2 CSI-OPEN)
            (define seq (read-csi-seq b2))
            (or (csi-to-key seq) (key-sym 'escape))]
           [(= b2 SS3)
            (define b3 (read-stdin-byte/timeout ESCDELAY))
            (if b3 (or (ss3-to-key b3) (key-sym 'escape))
                (key-sym 'escape))]
           [(<= SPACE b2 DELETE)
            ;; ESC <printable> → Meta (not supported yet, treat as escape)
            (key-sym 'escape)]
           [else (key-sym 'escape)])]

    ;; Control characters
    [(= b TAB)       (key-sym 'tab)]
    [(= b CR)        (key-sym 'return)]
    [(= b LF)        (key-sym 'return)]
    [(= b BACKSPACE) (key-sym 'backspace)]
    [(= b DELETE)    (key-sym 'backspace)]
    [(<= 0 b 31)     (key-ctrl (integer->char (+ b 96)))]  ;; 1→#\a, 3→#\c

    ;; Printable ASCII
    [(<= SPACE b DELETE) (key-char (integer->char b))]

    ;; UTF-8
    [(utf8-multi-start? b)
     (define rest (read-n/timeout (sub1 (utf8-length b))))
     (define full (bytes-append (bytes b) rest))
     (define str (bytes->string/utf-8 full))
     (if (= (string-length str) 1)
         (key-char (string-ref str 0))
         (key-sym 'unknown))]

    [else (key-sym 'unknown)]))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)

  (test-case "struct types"
    (check-pred key-char? (key-char #\a))
    (check-pred key-ctrl? (key-ctrl #\c))
    (check-pred key-sym?  (key-sym 'up)))

  (test-case "idle and quit"
    (check-true (key-idle? (key-sym 'idle)))
    (check-true (key-quit? (key-ctrl #\c)))
    (check-false (key-quit? (key-char #\c)))))
