#lang racket

;; kernel/data/query.rkt — Character-level queries & scanning on gap buffer
;;
;; ============================================================================
;; Composes gap.rkt (byte-level) + utf8.rkt (encode/decode).
;; All functions are PURE QUERIES — no mutation.
;; ============================================================================
;;
;;   ── Character Access ──
;;     gap-char            gap-buffer? byte-pos → char?
;;     gap-char+len        gap-buffer? byte-pos → (values char? byte-len)
;;
;;   ── String Extraction ──
;;     gap-substring       gap-buffer? byte-pos byte-pos → string?
;;     gap-string          gap-buffer? → string?
;;
;;   ── Navigation ──
;;     gap-next-char-pos   gap-buffer? byte-pos → byte-pos
;;     gap-prev-char-pos   gap-buffer? byte-pos → byte-pos
;;     gap-char-start      gap-buffer? byte-pos → byte-pos  (safety net)
;;     gap-skip-n          gap-buffer? byte-pos exact-nonnegative-integer? → byte-pos
;;
;;   ── Scanning ──
;;     gap-scan-byte       gap-buffer? byte-pos direction (byte? → bool) → byte-pos
;;     gap-scan-char       gap-buffer? byte-pos direction (char? → bool) → byte-pos
;;
;;   ── Match ──
;;     gap-match-str-at    gap-buffer? byte-pos string? → boolean?
;;     gap-at-bol?         gap-buffer? byte-pos → boolean?
;;     gap-read-delim-word gap-buffer? byte-pos → (values string? byte-pos)
;;
;; ============================================================================
;; Scanning Direction
;; ============================================================================
;;
;;   'forward  : scan from pos forward (inclusive at pos).
;;               Returns first match or gap-length (past-the-end).
;;   'backward : scan from pos backward (exclusive at pos).
;;               Returns last match or -1 (before-beginning).
;;               For gap-scan-char, the scan is character-by-character
;;               backward — it finds the character before pos that
;;               satisfies pred.
;;
;; ============================================================================

(require "gap.rkt"
         "utf8.rkt")

(provide
 ;; ── character access ──
 gap-char
 gap-char+len

 ;; ── string extraction ──
 gap-substring
 gap-string

 ;; ── navigation ──
 gap-next-char-pos
 gap-prev-char-pos
 gap-char-start
 gap-skip-n

 ;; ── scanning ──
 gap-scan-byte
 gap-scan-char

 ;; ── match ──
 gap-match-str-at
 gap-at-bol?
 gap-read-delim-word)

;; ============================================================
;; Character Access
;; ============================================================

(define (gap-char+len gb pos)
  ;; Decoded character + its UTF-8 byte length.
  ;; Contract: pos must be a valid logical position < gap-length.
  (define phys (physical-index gb pos))
  (define bs (gap-buffer-bytes gb))
  (utf8-decode bs phys))

(define (gap-char gb pos)
  ;; Decoded character at logical byte-pos.
  ;; Contract: pos must be a valid logical position < gap-length.
  (define-values (ch _len) (gap-char+len gb pos))
  ch)

;; ============================================================
;; String Extraction
;; ============================================================

(define (gap-substring gb from to)
  ;; Decode bytes [from, to) as a UTF-8 string.
  ;; Contract: [from, to) must be a valid logical range.
  (unless (gap-valid-range? gb from to)
    (raise-argument-error 'gap-substring
                          (format "valid range in [0, ~a]" (gap-length gb))
                          (list from to)))
  (bytes->string/utf-8 (gap-subbytes gb from to)))

(define (gap-string gb)
  ;; Full buffer content as a string.
  (gap-substring gb 0 (gap-length gb)))

;; ============================================================
;; Navigation
;; ============================================================

(define (gap-next-char-pos gb pos)
  ;; Byte position of the next character after pos.
  ;; Contract: pos must be a valid logical position < gap-length.
  ;; Correctly handles gap crossing.
  (define bs  (gap-buffer-bytes gb))
  (define gs  (gap-buffer-gap-start gb))
  (define ge  (gap-buffer-gap-end gb))
  (define phys (physical-index gb pos))
  (define next-phys (utf8-next-pos bs phys))

  ;; If next position falls inside the gap, skip to after the gap.
  (define adjusted
    (if (and (>= next-phys gs) (< next-phys ge))
        ge
        next-phys))
  (logical-index gb adjusted))

(define (gap-prev-char-pos gb pos)
  ;; Byte position of the character immediately before pos.
  ;; Contract: pos must be a valid logical position.
  ;; Returns 0 if pos is 0 (nothing before the first character).
  ;;
  ;; The gap adds complexity: walking backward through physical bytes
  ;; may cross the gap boundary.  We handle this by detecting when
  ;; we've landed on or crossed the gap, and jumping appropriately.
  (cond
    [(zero? pos) 0]

    [else
     (define bs  (gap-buffer-bytes gb))
     (define gs  (gap-buffer-gap-start gb))
     (define ge  (gap-buffer-gap-end gb))
     (define phys (physical-index gb pos))

     ;; Walk backward over continuation bytes.
     ;; If we land inside the gap or cross it, jump to just before it.
     (define prev-phys
       (let loop ([p (sub1 phys)])
         (cond
           [(< p 0) 0]
           ;; Just crossed gap boundary from the right → jump to before gap
           [(and (> ge gs) (= p (sub1 ge)))
            (loop (sub1 gs))]
           ;; Landed inside the gap → jump to before gap
           [(and (> ge gs) (>= p gs) (< p ge))
            (loop (sub1 gs))]
           [(utf8-start-byte? (bytes-ref bs p)) p]
           [else (loop (sub1 p))])))

     (logical-index gb prev-phys)]))

(define (gap-char-start gb byte-pos)
  ;; Snap byte-pos to a valid UTF-8 character boundary.
  ;; Call this safety-net function before any byte-position-sensitive
  ;; operation to ensure you're not in the middle of a multi-byte char.
  ;; Contract: byte-pos must be a valid logical position.
  (define bs (gap-buffer-bytes gb))
  (define phys (physical-index gb byte-pos))
  (define snapped-phys (utf8-char-start bs phys))
  (logical-index gb snapped-phys))

(define (gap-skip-n gb pos n)
  ;; Advance `pos` forward by `n` characters.
  ;; Returns the byte position after skipping `n` chars.
  ;; Clamped to gap-length if past the end.
  ;; Contract: pos must be a valid logical position.
  (unless (and (exact-nonnegative-integer? n)
               (gap-valid-pos? gb pos))
    (raise-argument-error 'gap-skip-n
                          (format "valid position in [0, ~a] and non-negative n"
                                  (gap-length gb))
                          (list pos n)))
  (let loop ([p pos] [i n])
    (if (or (zero? i) (>= p (gap-length gb)))
        p
        (loop (gap-next-char-pos gb p) (sub1 i)))))

;; ============================================================
;; Scanning — Byte Level
;; ============================================================

(define (gap-scan-byte gb pos direction pred)
  ;; Scan for a byte where `pred` is #t.
  ;; 'forward:  returns position of first match at or after `pos`.
  ;; 'backward: returns position of last match before `pos`.
  ;; Returns length (forward) or -1 (backward) if not found.
  (define len (gap-length gb))
  (case direction
    [(forward)
     (let loop ([p pos])
       (cond [(>= p len) len]
             [(pred (gap-byte-ref gb p)) p]
             [else (loop (add1 p))]))]
    [(backward)
     (let loop ([p (sub1 pos)])
       (cond [(< p 0) -1]
             [(pred (gap-byte-ref gb p)) p]
             [else (loop (sub1 p))]))]
    [else
     (raise-argument-error 'gap-scan-byte
                           "'forward or 'backward"
                           direction)]))

;; ============================================================
;; Scanning — Character Level
;; ============================================================

(define (gap-scan-char gb pos direction pred)
  ;; Scan for a character where `pred` is #t.
  ;; 'forward:  returns position of first matching char at or after `pos`.
  ;; 'backward: returns position of last matching char before `pos`.
  ;; Returns length (forward) or -1 (backward) if not found.
  (define len (gap-length gb))
  (case direction
    [(forward)
     (let loop ([p pos])
       (cond [(>= p len) len]
             [else
              (define-values (ch clen) (gap-char+len gb p))
              (if (pred ch) p (loop (+ p clen)))]))]
    [(backward)
     (let loop ([p pos])
       (define prev (gap-prev-char-pos gb p))
       (cond [(<= prev 0)
              (if (and (= prev 0) (< 0 pos))
                  (let ([ch (gap-char gb 0)])
                    (if (pred ch) 0 -1))
                  -1)]
             [(< prev pos)
              (define ch (gap-char gb prev))
              (if (pred ch) prev (loop prev))]
             [else -1]))]
    [else
     (raise-argument-error 'gap-scan-char
                           "'forward or 'backward"
                           direction)]))

;; ============================================================
;; Match Helpers
;; ============================================================

(define (gap-match-str-at gb pos str)
  ;; Does the text at byte-pos match `str` exactly?
  ;; Compares character-by-character (handles multi-byte).
  (unless (and (gap-valid-pos? gb pos)
               (string? str))
    (raise-argument-error 'gap-match-str-at
                          "valid position and string"
                          (list pos str)))
  (define slen (string-length str))
  (define buflen (gap-length gb))
  ;; Quick rejection: not enough bytes remaining
  (and (<= (+ pos slen) buflen)   ;; character count, not byte count — conservative
       (let loop ([i 0] [p pos])
         (if (= i slen)
             #t
             (and (< p buflen)
                  (char=? (gap-char gb p) (string-ref str i))
                  (loop (add1 i) (gap-next-char-pos gb p)))))))

(define (gap-at-bol? gb pos)
  ;; Is `pos` at the beginning of a line?
  ;; True if pos==0 or the character before pos is a newline.
  (or (zero? pos)
      (and (gap-valid-pos? gb pos)
           (> (gap-length gb) 0)
           (char=? (gap-char gb (gap-prev-char-pos gb pos)) #\newline))))

(define (gap-read-delim-word gb pos)
  ;; Read a non-whitespace word starting at `pos`.
  ;; Returns (values word-string end-byte-pos).
  ;; Used for heredoc delimiter capture (#<<HERE).
  (unless (gap-valid-pos? gb pos)
    (raise-argument-error 'gap-read-delim-word
                          (format "valid position in [0, ~a]" (gap-length gb))
                          pos))
  (define buflen (gap-length gb))
  (let loop ([p pos] [chars '()])
    (if (>= p buflen)
        (values (list->string (reverse chars)) p)
        (let ([ch (gap-char gb p)])
          (if (or (char=? ch #\space) (char=? ch #\tab)
                  (char=? ch #\newline) (char=? ch #\return))
              (values (list->string (reverse chars)) p)
              (loop (gap-next-char-pos gb p) (cons ch chars)))))))
