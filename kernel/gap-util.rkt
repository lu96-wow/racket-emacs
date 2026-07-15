#lang racket

;; kernel/gap-util.rkt — Pure gap-buffer helper functions
;;
;; Shared by font-lock, syntax-cache, and other scanning modules.
;; All functions are pure computations on gap-buffer — no mutation, no IO.

(require "gap.rkt")

(provide
 char-at
 char-len
 skip-n
 match-str-at
 at-bol?
 read-delim-word)

;; ============================================================
;; Single-character access
;; ============================================================

(define (char-at gb pos)
  (let-values ([(ch _len) (gap-char-at gb pos)]) ch))

(define (char-len gb pos)
  (let-values (([_ch len] (gap-char-at gb pos))) len))

;; ============================================================
;; Skip N characters forward
;; ============================================================

(define (skip-n gb pos n)
  (let loop ([p pos] [i n])
    (if (zero? i) p (loop (+ p (char-len gb p)) (sub1 i)))))

;; ============================================================
;; Match string at byte position
;; ============================================================

(define (match-str-at gb pos buflen s)
  (define slen (string-length s))
  (and (<= (+ pos slen) buflen)
       (let loop ([i 0] [p pos])
         (if (= i slen)
             #t
             (let-values ([(ch _cl) (gap-char-at gb p)])
               (and (char=? ch (string-ref s i))
                    (loop (add1 i) (+ p (char-len gb p)))))))))

;; ============================================================
;; Beginning-of-line check
;; ============================================================

(define (at-bol? gb pos)
  (or (zero? pos)
      (let ([prev (gap-prev-char-pos gb pos)])
        (and prev (char=? (char-at gb prev) #\newline)))))

;; ============================================================
;; Read a non-whitespace word (for heredoc delimiter capture)
;; ============================================================

(define (read-delim-word gb pos buflen)
  (let loop ([p pos] [chars '()])
    (if (>= p buflen)
        (values (list->string (reverse chars)) p)
        (let*-values ([(ch _cl) (gap-char-at gb p)])
          (if (or (char=? ch #\space) (char=? ch #\tab)
                  (char=? ch #\newline) (char=? ch #\return))
              (values (list->string (reverse chars)) p)
              (loop (+ p (char-len gb p)) (cons ch chars)))))))
