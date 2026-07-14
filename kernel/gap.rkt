#lang racket

;; base/gap.rkt — Bytes-Based Gap Buffer
;;
;; Layout:  [text-before]  [gap]  [text-after]
;;          ↑ 0         ↑ gap-start  ↑ gap-end  ↑ bytes-length
;;
;; All positions are byte offsets into the logical UTF-8 byte stream.
;; #\newline is always 0x0A (single byte) → line scanning is byte-at-a-time.
;;
;; Key types:
;;   byte-pos = exact-nonnegative-integer?  — byte offset in logical buffer
;;   char     = char?                       — decoded Unicode codepoint
;;   char-len = (integer-in 1 4)            — UTF-8 byte count of one char

(provide
 ;; ── constructor ──
 make-gap-buffer gap-buffer?
 gap-buffer-bytes gap-buffer-gap-start gap-buffer-gap-end

 ;; ── byte-level queries ──
 gap-byte-ref         ; O(1) single byte at byte-pos
 gap-byte-length      ; total byte count (excluding gap)

 ;; ── char-level queries ──
 gap-char-at          ; decode char at byte-pos → (values char char-len)
 gap-prev-char-pos    ; byte-pos of the character before given byte-pos
 gap-subbytes         ; raw bytes in [from, to)
 gap-substring        ; decoded string in [from, to)

 ;; ── byte-level scanning (for #\newline, #\space etc.) ──
 gap-scan-forward-byte
 gap-scan-backward-byte

 ;; ── char-level scanning (predicate runs on decoded char) ──
 gap-scan-forward-char
 gap-scan-backward-char

 ;; ── mutation (internal — used by buffer.rkt) ──
 gap-insert-bytes!
 gap-delete-range!)

;; ============================================================
;; UTF-8 helpers
;; ============================================================

(define (utf-8-start-byte? b)
  (not (= (bitwise-and b #xC0) #x80)))

(define (utf-8-char-len b)
  (cond [(< b #x80) 1]
        [(< b #xE0) 2]
        [(< b #xF0) 3]
        [else      4]))

(define (bytes-utf-8-ref bs byte-pos)
  ;; Decode one UTF-8 character from raw bytes at byte-pos.
  ;; Returns (values char consumed-byte-count).
  ;; Assumes byte-pos points to a valid UTF-8 start byte.
  (define b0 (bytes-ref bs byte-pos))
  (if (< b0 #x80)
      ;; ASCII fast-path — 99% of program text
      (values (integer->char b0) 1)
      (let* ([len  (utf-8-char-len b0)]
             [seg  (subbytes bs byte-pos (+ byte-pos len))]
             [str  (bytes->string/utf-8 seg)])
        (values (string-ref str 0) len))))

;; ============================================================
;; Struct
;; ============================================================

(struct gap-buffer
  ([bytes #:mutable]     ; bytes? — includes gap region
   [gap-start #:mutable] ; byte-pos — first byte of gap
   [gap-end #:mutable])  ; byte-pos — first byte after gap
  #:transparent)

;; ============================================================
;; Constructor
;; ============================================================

(define (make-gap-buffer [initial-text ""])
  (define init-bs (string->bytes/utf-8 initial-text))
  (define init-len (bytes-length init-bs))
  (define cap (max 256 (* 2 init-len)))
  (define bs (make-bytes cap 0))
  (bytes-copy! bs 0 init-bs)
  ;; Initial gap: right after the text, filling the rest
  (gap-buffer bs init-len cap))

;; ============================================================
;; Internal: physical index mapping
;; ============================================================

(define (gap-physical-index gb logical-pos)
  (if (< logical-pos (gap-buffer-gap-start gb))
      logical-pos
      (+ logical-pos (- (gap-buffer-gap-end gb) (gap-buffer-gap-start gb)))))

;; ============================================================
;; Byte-level queries
;; ============================================================

(define (gap-byte-ref gb byte-pos)
  (bytes-ref (gap-buffer-bytes gb) (gap-physical-index gb byte-pos)))

(define (gap-byte-length gb)
  (- (bytes-length (gap-buffer-bytes gb))
     (- (gap-buffer-gap-end gb) (gap-buffer-gap-start gb))))

;; ============================================================
;; Char-level queries
;; ============================================================

(define (gap-char-at gb byte-pos)
  ;; Decode one character at logical byte-pos. Assumes byte-pos
  ;; is at a valid UTF-8 start byte (the gap is always character-aligned).
  ;; Returns (values char consumed-byte-count).
  (define bs (gap-buffer-bytes gb))
  (define phys (gap-physical-index gb byte-pos))
  (bytes-utf-8-ref bs phys))

(define (gap-subbytes gb from to)
  ;; Raw bytes in [from, to). Handles gap crossing.
  (define gs (gap-buffer-gap-start gb))
  (define ge (gap-buffer-gap-end gb))
  (define len (gap-byte-length gb))
  (define real-to (min to len))
  (define bs (gap-buffer-bytes gb))
  (cond
    [(<= real-to gs)
     ;; Entirely before gap
     (subbytes bs from real-to)]
    [(>= from gs)
     ;; Entirely after gap
     (subbytes bs (+ from (- ge gs)) (+ real-to (- ge gs)))]
    [else
     ;; Spans the gap
     (bytes-append
      (subbytes bs from gs)
      (subbytes bs ge (+ real-to (- ge gs))))]))

(define (gap-substring gb from to)
  (bytes->string/utf-8 (gap-subbytes gb from to)))

;; ============================================================
;; Byte-level scanning
;; ============================================================

(define (gap-scan-forward-byte gb byte-pos pred)
  ;; pred: byte? → boolean?
  ;; Scan forward from byte-pos, return first byte-pos where pred is #t.
  ;; Returns gap-byte-length if not found.
  (define len (gap-byte-length gb))
  (let loop ([pos byte-pos])
    (cond [(>= pos len) len]
          [(pred (gap-byte-ref gb pos)) pos]
          [else (loop (add1 pos))])))

(define (gap-scan-backward-byte gb byte-pos pred)
  ;; Scan backward from (sub1 byte-pos), return first byte-pos where pred
  ;; is #t. Returns -1 if not found.
  (let loop ([pos (sub1 byte-pos)])
    (cond [(< pos 0) -1]
          [(pred (gap-byte-ref gb pos)) pos]
          [else (loop (sub1 pos))])))

;; ============================================================
;; Char-level scanning
;; ============================================================

(define (gap-scan-forward-char gb byte-pos pred)
  (define len (gap-byte-length gb))
  (let loop ([pos byte-pos])
    (cond [(>= pos len) len]
          [else
           (define-values (ch clen) (gap-char-at gb pos))
           (if (pred ch) pos (loop (+ pos clen)))])))

;; Helper: find the start byte of the character BEFORE byte-pos.
;; Handles UTF-8 multi-byte and gap crossing correctly.
(define (gap-prev-char-pos gb byte-pos)
  (define bs (gap-buffer-bytes gb))
  (define gs (gap-buffer-gap-start gb))
  (define ge (gap-buffer-gap-end gb))
  (define phys (if (< byte-pos gs) byte-pos (+ byte-pos (- ge gs))))
  ;; Walk backward from phys-1, jumping over the gap if necessary.
  (let loop ([p (sub1 phys)])
    (cond [(< p 0) 0]
          ;; Hit the gap boundary (scanning into gap from the right edge).
          ;; Jump to the last byte before the gap.
          [(and (> ge gs) (= p (sub1 ge)))
           (loop (sub1 gs))]
          [(utf-8-start-byte? (bytes-ref bs p))
           ;; Found a UTF-8 start byte. Convert physical → logical.
           (if (< p gs) p (- p (- ge gs)))]
          [else (loop (sub1 p))])))

(define (gap-scan-backward-char gb byte-pos pred)
  ;; Scan backward character by character. Returns byte-pos of first match,
  ;; or -1 if not found.
  (let loop ([pos byte-pos])
    (define prev (gap-prev-char-pos gb pos))
    (cond [(<= prev 0)
           (if (and (= prev 0) (< 0 byte-pos))
               (let-values ([(ch clen) (gap-char-at gb 0)])
                 (if (pred ch) 0 -1))
               -1)]
          [(< prev byte-pos)
           (define-values (ch clen) (gap-char-at gb prev))
           (if (pred ch) prev (loop prev))]
          [else -1])))

;; ============================================================
;; Mutation: gap-move-to!
;; ============================================================

(define (gap-move-to! gb byte-pos)
  ;; Move the gap so it starts at byte-pos.
  ;; byte-pos must be a valid UTF-8 start byte.
  (define gs (gap-buffer-gap-start gb))
  (define ge (gap-buffer-gap-end gb))
  (cond
    [(< byte-pos gs)
     ;; Move gap left: copy [byte-pos, gs) to end of gap area
     (define shift (- gs byte-pos))
     (define bs (gap-buffer-bytes gb))
     (define new-ge (- ge shift))
     (bytes-copy! bs new-ge bs byte-pos gs)
     (set-gap-buffer-gap-start! gb byte-pos)
     (set-gap-buffer-gap-end! gb new-ge)]
    [(> byte-pos gs)
     ;; Move gap right: copy [ge, ge+shift) to gap-start
     (define shift (- byte-pos gs))
     (define bs (gap-buffer-bytes gb))
     (define new-ge (+ ge shift))
     (bytes-copy! bs gs bs ge new-ge)
     (set-gap-buffer-gap-start! gb byte-pos)
     (set-gap-buffer-gap-end! gb new-ge)]
    [else (void)]))

;; ============================================================
;; Mutation: gap-reserve!
;; ============================================================

(define (gap-reserve! gb need)
  (define gs (gap-buffer-gap-start gb))
  (define ge (gap-buffer-gap-end gb))
  (define gap-size (- ge gs))
  (when (< gap-size need)
    (define old-bs (gap-buffer-bytes gb))
    (define old-len (bytes-length old-bs))
    (define new-cap (max (* 2 old-len) (+ old-len need)))
    (define new-bs (make-bytes new-cap 0))
    ;; Copy text before gap
    (bytes-copy! new-bs 0 old-bs 0 gs)
    ;; Copy text after gap (to end of new buffer)
    (define new-ge (- new-cap (- old-len ge)))
    (bytes-copy! new-bs new-ge old-bs ge old-len)
    (set-gap-buffer-bytes! gb new-bs)
    (set-gap-buffer-gap-end! gb new-ge)))

;; ============================================================
;; Mutation: insert / delete
;; ============================================================

(define (gap-insert-bytes! gb byte-pos bs)
  ;; Insert raw bytes at logical byte-pos.
  (define blen (bytes-length bs))
  (when (positive? blen)
    (gap-move-to! gb byte-pos)
    (gap-reserve! gb blen)
    (define gs (gap-buffer-gap-start gb))
    (define buf (gap-buffer-bytes gb))
    (bytes-copy! buf gs bs)
    (set-gap-buffer-gap-start! gb (+ gs blen))))

(define (gap-delete-range! gb from to)
  ;; Delete bytes in [from, to). Both are byte-pos.
  (define count (- to from))
  (when (positive? count)
    (gap-move-to! gb from)
    ;; Expand gap to swallow the deleted range
    (set-gap-buffer-gap-end! gb (+ (gap-buffer-gap-end gb) count))))

;; ============================================================
;; Debug
;; ============================================================

(module+ test
  (require rackunit)

  ;; Basic construction
  (let ([gb (make-gap-buffer "hello\nworld")])
    (check-equal? (gap-byte-length gb) 11)
    (check-equal? (gap-substring gb 0 11) "hello\nworld"))

  ;; Byte scanning for newline
  (let ([gb (make-gap-buffer "ab\ncd")])
    (check-equal? (gap-scan-forward-byte gb 0 (curry = #x0A)) 2)
    (check-equal? (gap-scan-backward-byte gb 5 (curry = #x0A)) 2))

  ;; Insert + UTF-8
  (let ([gb (make-gap-buffer "ab")])
    (gap-insert-bytes! gb 2 (string->bytes/utf-8 "你好"))
    (check-equal? (gap-byte-length gb) 8)  ; 2 + 3*2
    (check-equal? (gap-substring gb 0 8) "ab你好")
    (define-values (ch len) (gap-char-at gb 2))
    (check-equal? ch #\你)
    (check-equal? len 3))

  ;; Delete
  (let ([gb (make-gap-buffer "abcdef")])
    (gap-delete-range! gb 1 3)
    (check-equal? (gap-substring gb 0 4) "adef"))

  ;; Char scan
  (let ([gb (make-gap-buffer "abc你def")])
    (define (word-char? ch) (char-alphabetic? ch))
    ;; Forward: from 0, skip 'abc' → '你' (CJK, not word-char)
    (define p1 (gap-scan-forward-char gb 0 (negate word-char?)))
    (check-equal? (gap-substring gb 0 p1) "abc"))
    ;; Backward: from end
    (let ([gb2 (make-gap-buffer "abc你好def")])
      (define len (gap-byte-length gb2))
      (define p (gap-scan-backward-char gb2 len char-alphabetic?))
      (check-true (> p 0)))
  )
