#lang racket

;; kernel/data/utf8.rkt — UTF-8 encode/decode (pure functions, zero deps)
;;
;; ============================================================================
;; Computation Only — zero mutation, zero allocation beyond return values
;; ============================================================================
;;
;;   Classification:
;;     utf8-start-byte?      — is this byte the first byte of a character?
;;     utf8-char-len          — how many bytes for this start byte?
;;
;;   Encode / Decode:
;;     utf8-encode            — Racket char → bytes?
;;     utf8-decode            — bytes? byte-pos → (values char char-len)
;;     utf8-decode-reverse    — decode character ENDING at byte-pos
;;
;;   Navigation (on raw bytes, no gap needed):
;;     utf8-next-pos          — byte position of next character
;;     utf8-prev-pos          — byte position of previous character
;;     utf8-char-start        — snap any byte position to char boundary
;;     utf8-valid-sequence?   — validate a UTF-8 byte sequence
;;
;; ============================================================================
;; Contract
;; ============================================================================
;;
;;   Functions that decode/encode validate their inputs.
;;   Navigation functions expect valid UTF-8 byte positions.
;;   `utf8-char-start` is the safety net: call it before any
;;   position-sensitive operation to avoid mid-character positions.
;;
;; ============================================================================

(provide
 ;; ── classification ──
 utf8-start-byte?
 utf8-char-len
 utf8-continuation-byte?

 ;; ── encode / decode ──
 utf8-encode
 utf8-decode
 utf8-decode-reverse

 ;; ── navigation ──
 utf8-next-pos
 utf8-prev-pos
 utf8-char-start

 ;; ── validation ──
 utf8-valid-sequence?)

;; ============================================================
;; Classification
;; ============================================================

(define (utf8-start-byte? b)
  ;; Is `b` the first byte of a UTF-8 character?
  ;; Continuation bytes (10xxxxxx) and invalid bytes return #f.
  (not (= (bitwise-and b #xC0) #x80)))

(define (utf8-continuation-byte? b)
  ;; Is `b` a UTF-8 continuation byte (10xxxxxx)?
  (= (bitwise-and b #xC0) #x80))

(define (utf8-char-len b)
  ;; How many bytes does the character starting with `b` occupy?
  ;; Assumes `b` is a valid UTF-8 start byte.
  (cond [(< b #x80) 1]          ;; 0xxxxxxx — ASCII
        [(< b #xE0) 2]          ;; 110xxxxx — 2-byte
        [(< b #xF0) 3]          ;; 1110xxxx — 3-byte
        [else       4]))        ;; 11110xxx — 4-byte

;; ============================================================
;; Encode
;; ============================================================

(define (utf8-encode ch)
  ;; Convert a single Racket character to a UTF-8 byte string.
  ;; Returns bytes? of length 1–4.
  ;; Contract: ch must be a valid Racket character.
  (unless (char? ch)
    (raise-argument-error 'utf8-encode "char?" ch))
  (string->bytes/utf-8 (string ch)))

;; ============================================================
;; Decode
;; ============================================================

(define (utf8-decode bs byte-pos)
  ;; Decode one UTF-8 character from raw bytes at byte-pos.
  ;; Contract: byte-pos must point to a valid UTF-8 start byte.
  ;; Returns (values char consumed-byte-count).
  (unless (and (bytes? bs)
               (exact-nonnegative-integer? byte-pos)
               (< byte-pos (bytes-length bs)))
    (raise-argument-error 'utf8-decode
                          "valid byte position in bytes"
                          byte-pos))

  (define b0 (bytes-ref bs byte-pos))

  ;; ASCII fast-path — 99% of program text
  (if (< b0 #x80)
      (values (integer->char b0) 1)
      ;; Multi-byte: validate, then decode
      (let* ([len      (utf8-char-len b0)]
             [end      (+ byte-pos len)]
             [_        (unless (and (<= end (bytes-length bs))
                                    (utf8-valid-sequence? bs byte-pos len))
                         (raise-arguments-error
                          'utf8-decode
                          "invalid UTF-8 sequence at position"
                          "position" byte-pos
                          "length" len))]
             [seg      (subbytes bs byte-pos end)]
             [str      (bytes->string/utf-8 seg)])
        (values (string-ref str 0) len))))

(define (utf8-decode-reverse bs byte-pos)
  ;; Decode the character that ENDS at byte-pos.
  ;; byte-pos is the position AFTER the last byte of the character.
  ;; Walks backward to find the start byte, then decodes forward.
  (define start (utf8-prev-pos bs byte-pos))
  (utf8-decode bs start))

;; ============================================================
;; Navigation (on raw bytes)
;; ============================================================

(define (utf8-next-pos bs byte-pos)
  ;; Position of the next character after the one at byte-pos.
  ;; Contract: byte-pos must be a valid UTf-8 start byte or at end.
  ;; Returns (min (+ byte-pos char-len) bytes-length).
  (unless (and (bytes? bs)
               (exact-nonnegative-integer? byte-pos)
               (< byte-pos (bytes-length bs)))
    (raise-argument-error 'utf8-next-pos
                          "valid byte position in bytes"
                          byte-pos))
  (min (+ byte-pos (utf8-char-len (bytes-ref bs byte-pos)))
       (bytes-length bs)))

(define (utf8-prev-pos bs byte-pos)
  ;; Byte position of the character boundary just before byte-pos.
  ;; Walks backward over continuation bytes (10xxxxxx).
  ;; Contract: byte-pos must be a valid position in [0, bytes-length].
  ;; Returns 0 if no previous character.
  (unless (and (bytes? bs)
               (exact-nonnegative-integer? byte-pos)
               (<= byte-pos (bytes-length bs)))
    (raise-argument-error 'utf8-prev-pos
                          "valid position in [0, bytes-length]"
                          byte-pos))
  (let loop ([p (sub1 byte-pos)])
    (cond [(< p 0) 0]
          [(utf8-start-byte? (bytes-ref bs p)) p]
          [else (loop (sub1 p))])))

(define (utf8-char-start bs byte-pos)
  ;; Snap byte-pos to the nearest valid UTF-8 character start.
  ;; If already at a start byte (or at end), return unchanged.
  ;; If in the middle of a multi-byte char, walk back to start.
  ;; This is the SAFETY NET: call before any byte-position-sensitive
  ;; operation to avoid mid-character positions.
  (unless (and (bytes? bs)
               (exact-nonnegative-integer? byte-pos)
               (<= byte-pos (bytes-length bs)))
    (raise-argument-error 'utf8-char-start
                          "valid position in [0, bytes-length]"
                          byte-pos))
  (if (or (>= byte-pos (bytes-length bs))
          (utf8-start-byte? (bytes-ref bs byte-pos)))
      byte-pos
      (utf8-prev-pos bs (add1 byte-pos))))

;; ============================================================
;; utf8-valid-sequence? — validate a UTF-8 sequence
;; ============================================================

(define (utf8-valid-sequence? bs start len)
  ;; Is the byte sequence at bs[start] of length `len` valid UTF-8?
  ;; Checks:
  ;;   - Correct continuation bytes after start byte
  ;;   - No overlong encoding
  ;;   - No surrogates (U+D800–U+DFFF)
  ;;   - No code points > U+10FFFF
  (define end (+ start len))
  (unless (and (>= start 0) (<= end (bytes-length bs)))
    (raise-argument-error 'utf8-valid-sequence?
                          "valid byte range"
                          (list start len)))

  (define b0 (bytes-ref bs start))

  (cond
    ;; 1-byte (ASCII): always valid
    [(= len 1) (or (< b0 #x80) (not (utf8-continuation-byte? b0)))]

    ;; 2-byte: b0 = 110xxxxx, check continuation byte
    [(= len 2)
     (and (= (bitwise-and b0 #xE0) #xC0)         ;; 110xxxxx
          (>= b0 #xC2)                             ;; no overlong: 0xC0/0xC1 invalid
          (utf8-continuation-byte? (bytes-ref bs (add1 start))))]

    ;; 3-byte: b0 = 1110xxxx, check continuation bytes + no surrogates
    [(= len 3)
     (and (= (bitwise-and b0 #xF0) #xE0)          ;; 1110xxxx
          (not (and (= b0 #xED)                    ;; no surrogates (ED A0–BF)
                    (>= (bytes-ref bs (add1 start)) #xA0)))
          (utf8-continuation-byte? (bytes-ref bs (add1 start)))
          (utf8-continuation-byte? (bytes-ref bs (+ start 2))))]

    ;; 4-byte: b0 = 11110xxx, check continuation bytes + max U+10FFFF
    [(= len 4)
     (and (= (bitwise-and b0 #xF8) #xF0)          ;; 11110xxx
          (<= b0 #xF4)                              ;; max: F4 (U+10FFFF)
          (utf8-continuation-byte? (bytes-ref bs (add1 start)))
          (utf8-continuation-byte? (bytes-ref bs (+ start 2)))
          (utf8-continuation-byte? (bytes-ref bs (+ start 3))))]

    [else #f]))
