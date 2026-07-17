#lang racket

;; kernel/gap/query.rkt — Character-level queries & scanning on gap buffer
;;
;; Composes gap.rkt (byte-level) + utf8.rkt (encode/decode).
;; All functions are pure queries — no mutation.
;; Scanning is direction-parameterized: 'forward or 'backward.

(require "gap.rkt"
         "utf8.rkt")

(provide
 ;; character access
 gap-char
 gap-char+len

 ;; string extraction
 gap-substring          ; gap-buffer? byte-pos byte-pos -> string?
 gap-string             ; gap-buffer? -> string?

 ;; navigation
 gap-next-char-pos      ; gap-buffer? byte-pos -> byte-pos
 gap-prev-char-pos      ; gap-buffer? byte-pos -> byte-pos
 gap-skip-n             ; gap-buffer? byte-pos n -> byte-pos

 ;; scanning
 gap-scan-byte          ; gap-buffer? byte-pos direction (byte? -> bool) -> byte-pos
 gap-scan-char          ; gap-buffer? byte-pos direction (char? -> bool) -> byte-pos

 ;; match
 gap-match-str-at       ; gap-buffer? byte-pos string? -> boolean?
 gap-at-bol?            ; gap-buffer? byte-pos -> boolean?
 gap-read-delim-word    ; gap-buffer? byte-pos -> values: string?, byte-pos
)

;; ============================================================
;; Character access
;; ============================================================

(define (gap-char gb pos)
  ;; Decoded character at logical byte-pos.
  (define-values (ch _len) (gap-char+len gb pos))
  ch)

(define (gap-char+len gb pos)
  ;; Decoded character + its UTF-8 byte length.
  (define bs (gap-buffer-bytes gb))
  (utf8-decode bs (physical-index gb pos)))

;; ============================================================
;; String extraction
;; ============================================================

(define (gap-substring gb from to)
  (bytes->string/utf-8 (gap-subbytes gb from to)))

(define (gap-string gb)
  (gap-substring gb 0 (gap-length gb)))

;; ============================================================
;; Navigation
;; ============================================================

(define (gap-next-char-pos gb pos)
  (define bs (gap-buffer-bytes gb))
  (define gs (gap-buffer-gap-start gb))
  (define ge (gap-buffer-gap-end gb))
  (define phys (physical-index gb pos))
  (define next-phys (utf8-next-pos bs phys))
  ;; If next position falls inside the gap, skip to after the gap.
  (define adjusted
    (if (and (>= next-phys gs) (< next-phys ge))
        ge
        next-phys))
  (logical-index gb adjusted))

(define (gap-prev-char-pos gb pos)
  ;; If at position 0, there's nothing before — return 0 immediately.
  ;; This avoids the physical-to-logical round-trip that breaks when
  ;; the gap starts at position 0 (logical-index of physical 0 returns -245).
  (if (zero? pos) 0
      (let* ([bs  (gap-buffer-bytes gb)]
             [gs  (gap-buffer-gap-start gb)]
             [ge  (gap-buffer-gap-end gb)]
             [phys (physical-index gb pos)]
             ;; Walk backward over continuation bytes. If we cross into the gap
             ;; from the right side, jump to before the gap and continue.
             [prev-phys
              (let loop ([p (sub1 phys)])
                (cond [(< p 0) 0]
                      ;; Just crossed gap boundary from right → jump to before gap
                      [(and (> ge gs) (= p (sub1 ge)))
                       (loop (sub1 gs))]
                      ;; Landed inside gap → jump to before gap
                      [(and (> ge gs) (>= p gs) (< p ge))
                       (loop (sub1 gs))]
                      [(utf8-start-byte? (bytes-ref bs p)) p]
                      [else (loop (sub1 p))]))])
        (logical-index gb prev-phys))))

(define (gap-skip-n gb pos n)
  (let loop ([p pos] [i n])
    (if (zero? i) p (loop (gap-next-char-pos gb p) (sub1 i)))))

;; ============================================================
;; Scanning
;; ============================================================

(define (gap-scan-byte gb pos direction pred)
  ;; Scan for a byte where pred is #t. Returns byte-pos or boundary.
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
             [else (loop (sub1 p))]))]))

(define (gap-scan-char gb pos direction pred)
  ;; Scan for a character where pred is #t. Returns byte-pos or boundary.
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
             [else -1]))]))

;; ============================================================
;; Match helpers (used by syntax scanning)
;; ============================================================

(define (gap-match-str-at gb pos str)
  ;; Does the text at byte-pos match `str` exactly?
  (define slen (string-length str))
  (define buflen (gap-length gb))
  (and (<= (+ pos slen) buflen)
       (let loop ([i 0] [p pos])
         (if (= i slen)
             #t
             (let ([ch (gap-char gb p)])
               (and (char=? ch (string-ref str i))
                    (loop (add1 i) (gap-next-char-pos gb p))))))))

(define (gap-at-bol? gb pos)
  ;; Is pos at the beginning of a line?
  (or (zero? pos)
      (char=? (gap-char gb (gap-prev-char-pos gb pos)) #\newline)))

(define (gap-read-delim-word gb pos)
  ;; Read a non-whitespace word (for heredoc delimiter capture).
  ;; Returns (values word-string end-byte-pos).
  (define buflen (gap-length gb))
  (let loop ([p pos] [chars '()])
    (if (>= p buflen)
        (values (list->string (reverse chars)) p)
        (let ([ch (gap-char gb p)])
          (if (or (char=? ch #\space) (char=? ch #\tab)
                  (char=? ch #\newline) (char=? ch #\return))
              (values (list->string (reverse chars)) p)
              (loop (gap-next-char-pos gb p) (cons ch chars)))))))

;; ============================================================
;; Internal: index mapping
;; ============================================================

(define (physical-index gb logical-pos)
  (if (< logical-pos (gap-buffer-gap-start gb))
      logical-pos
      (+ logical-pos (- (gap-buffer-gap-end gb) (gap-buffer-gap-start gb)))))

(define (logical-index gb physical-pos)
  ;; Convert physical byte position → logical position.
  ;; Physical positions inside the gap are clamped to gap-start:
  ;; they don't correspond to any logical byte.
  (define gs (gap-buffer-gap-start gb))
  (define ge (gap-buffer-gap-end gb))
  (cond
    [(< physical-pos gs)   physical-pos]
    [(>= physical-pos ge)  (- physical-pos (- ge gs))]
    [else                  gs]))
