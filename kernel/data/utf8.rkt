#lang racket

;; kernel/gap/utf8.rkt — UTF-8 encode/decode (pure functions, zero deps)
;;
;; Character ↔ byte-sequence conversion.
;; All functions are pure — no allocation beyond the return value.
;; Used by query.rkt for character-level access on top of gap.rkt.

(provide
 ;; classification
 utf8-start-byte?
 utf8-char-len

 ;; encode / decode
 utf8-encode         ; char? -> bytes?
 utf8-decode         ; bytes? byte-pos -> (values char? char-len)
 utf8-decode-reverse ; bytes? byte-pos -> (values char? char-len)

 ;; navigation (on raw bytes, no gap needed)
 utf8-next-pos       ; bytes? byte-pos -> byte-pos
 utf8-prev-pos)      ; bytes? byte-pos -> byte-pos

;; ============================================================
;; Classification
;; ============================================================

(define (utf8-start-byte? b)
  ;; Is `b` the first byte of a UTF-8 character?
  ;; Continuation bytes have the form 10xxxxxx.
  (not (= (bitwise-and b #xC0) #x80)))

(define (utf8-char-len b)
  ;; How many bytes does the character starting with `b` occupy?
  ;; Assumes `b` is a valid UTF-8 start byte.
  (cond [(< b #x80) 1]
        [(< b #xE0) 2]
        [(< b #xF0) 3]
        [else      4]))

;; ============================================================
;; Encode / Decode
;; ============================================================

(define (utf8-encode ch)
  ;; Convert a single Racket character to a UTF-8 byte string.
  ;; Returns bytes? of length 1–4.
  (string->bytes/utf-8 (string ch)))

(define (utf8-decode bs byte-pos)
  ;; Decode one UTF-8 character from raw bytes at byte-pos.
  ;; Assumes byte-pos points to a valid UTF-8 start byte.
  ;; Returns (values char consumed-byte-count).
  (define b0 (bytes-ref bs byte-pos))
  (if (< b0 #x80)
      ;; ASCII fast-path — 99% of program text
      (values (integer->char b0) 1)
      (let* ([len  (utf8-char-len b0)]
             [seg  (subbytes bs byte-pos (+ byte-pos len))]
             [str  (bytes->string/utf-8 seg)])
        (values (string-ref str 0) len))))

(define (utf8-decode-reverse bs byte-pos)
  ;; Decode the character that ENDS at byte-pos.
  ;; Walks backward to find the start byte, then decodes forward.
  ;; byte-pos is the position AFTER the last byte of the character.
  (define start (utf8-prev-pos bs byte-pos))
  (utf8-decode bs start))

;; ============================================================
;; Navigation (on raw bytes)
;; ============================================================

(define (utf8-next-pos bs byte-pos)
  ;; Position of the next character after the one at byte-pos.
  ;; Returns (min (+ byte-pos char-len) bytes-length).
  (min (+ byte-pos (utf8-char-len (bytes-ref bs byte-pos)))
       (bytes-length bs)))

(define (utf8-prev-pos bs byte-pos)
  ;; Byte position of the character boundary just before byte-pos.
  ;; Walks backward over continuation bytes (10xxxxxx).
  ;; Returns 0 if no previous character.
  (let loop ([p (sub1 byte-pos)])
    (cond [(< p 0) 0]
          [(utf8-start-byte? (bytes-ref bs p)) p]
          [else (loop (sub1 p))])))
