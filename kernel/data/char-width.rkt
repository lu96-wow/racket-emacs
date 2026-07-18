#lang racket

;; kernel/data/char-width.rkt — Character display width (pure computation)
;;
;; ============================================================================
;; Pure Functions — no mutation, no gap dependency, no side effects.
;; ============================================================================
;;
;;   char-display-width     char? → -1 | 0 | 1 | 2
;;   gap-display-width      computes total display columns of a gap range
;;   scan-display-width     advance a byte position by at most N display columns
;;
;; ============================================================================
;; Display Width Classification
;; ============================================================================
;;
;;   -1  : control character (render as ^X, occupies 2 screen columns)
;;    0  : zero-width (combining marks, ZWJ, ZWNJ, etc.)
;;    1  : normal (most ASCII, Latin-1, etc.)
;;    2  : wide (CJK, emoji, etc.)
;;
;;   tab-width is parameterisable (default 8).
;;
;; ============================================================================

(require "gap.rkt"
         "query.rkt")

(provide
 char-display-width
 gap-display-width
 scan-display-width
 tab-width)

;; ============================================================
;; tab-width — parameterisable
;; ============================================================

(define tab-width (make-parameter 8))

;; ============================================================
;; char-display-width — terminal column count
;; ============================================================

(define (char-display-width ch)
  ;; Return the number of terminal columns this character occupies.
  ;; Returns -1 for control characters (rendered as ^X).
  ;; Contract: ch is a valid Racket character.
  (unless (char? ch)
    (raise-argument-error 'char-display-width "char?" ch))

  (define cp (char->integer ch))

  (cond
    ;; Control characters and DEL: render as ^X (2 columns)
    [(or (< cp 32) (= cp #x7F)) -1]

    ;; Tab: expands to tab-width spaces
    [(= cp 9) (tab-width)]

    ;; Zero-width combining marks, ZWJ, ZWNJ, BOM, etc.
    [(zero-width? cp) 0]

    ;; Wide characters (CJK, fullwidth, emoji)
    [(wide? cp) 2]

    ;; Everything else: single-width
    [else 1]))

;; ============================================================
;; Zero-width classification
;; ============================================================

(define (zero-width? cp)
  (or
   ;; Combining diacritical marks (U+0300–U+036F)
   (<= #x0300 cp #x036F)
   ;; Cyrillic combining marks
   (<= #x0483 cp #x0489)
   ;; Hebrew accents
   (<= #x0591 cp #x05BD) (= cp #x05BF)
   (<= #x05C1 cp #x05C2) (<= #x05C4 cp #x05C5) (= cp #x05C7)
   ;; Arabic marks
   (<= #x0610 cp #x061A) (<= #x064B cp #x065F) (= cp #x0670)
   ;; Syriac
   (= cp #x0711) (<= #x0730 cp #x074A)
   ;; Thaana
   (<= #x07A6 cp #x07B0)
   ;; Zero-width spaces and formatting characters
   (<= #x200B cp #x200F)
   (<= #x2028 cp #x202E)
   (<= #x2060 cp #x206F)
   ;; Variation selectors
   (<= #xFE00 cp #xFE0F) (= cp #xFEFF)
   ;; Interlinear annotation
   (<= #xFFF9 cp #xFFFB)
   ;; Emoji skin-tone modifiers
   (<= #x1F3FB cp #x1F3FF)
   ;; Tags
   (<= #xE0020 cp #xE007F)
   ;; Variation selectors supplement
   (<= #xE0100 cp #xE01EF)))

;; ============================================================
;; Wide classification
;; ============================================================

(define (wide? cp)
  (or
   ;; Hangul Jamo
   (<= #x1100 cp #x115F)
   ;; Angle brackets
   (<= #x2329 cp #x232A)
   ;; CJK radicals and symbols
   (<= #x2E80 cp #x303E)
   ;; Hiragana
   (<= #x3041 cp #x33BF)
   ;; CJK Unified Ideographs Extension A
   (<= #x3400 cp #x4DBF)
   ;; CJK Unified Ideographs
   (<= #x4E00 cp #xA4CF)
   ;; Hangul Syllables
   (<= #xA960 cp #xA97C)
   (<= #xAC00 cp #xD7A3)
   ;; CJK Compatibility Ideographs
   (<= #xF900 cp #xFAFF)
   ;; Vertical forms
   (<= #xFE10 cp #xFE19)
   (<= #xFE30 cp #xFE6F)
   ;; Fullwidth forms
   (<= #xFF01 cp #xFF60)
   (<= #xFFE0 cp #xFFE6)
   ;; Emoji and supplemental symbols (U+1F000+)
   (>= cp #x1F000)
   ;; CJK Unified Ideographs Extension B
   (<= #x20000 cp #x2FFFD)
   ;; CJK Unified Ideographs Extension C–G
   (<= #x30000 cp #x3FFFD)))

;; ============================================================
;; gap-display-width — total display columns of [from, to)
;; ============================================================

(define (gap-display-width gb from to)
  ;; Compute the total display column width of text in [from, to).
  ;; Contract: [from, to) must be a valid logical range.
  (unless (gap-valid-range? gb from to)
    (raise-argument-error 'gap-display-width
                          (format "valid range in [0, ~a]" (gap-length gb))
                          (list from to)))
  (let loop ([pos from] [w 0])
    (if (>= pos to)
        w
        (let-values ([(ch clen) (gap-char+len gb pos)])
          (loop (+ pos clen) (+ w (max 0 (char-display-width ch))))))))

;; ============================================================
;; scan-display-width — advance by at most max-width columns
;; ============================================================

(define (scan-display-width gb start end max-width)
  ;; Advance from `start` toward `end`, consuming at most `max-width`
  ;; display columns.  Returns the first byte position that would
  ;; exceed `max-width` (or `end` if the line is shorter).
  ;; Contract: [start, end) must be a valid logical range.
  (unless (and (gap-valid-range? gb start end)
               (exact-nonnegative-integer? max-width))
    (raise-argument-error 'scan-display-width
                          (format "valid range and non-negative max-width")
                          (list start end max-width)))
  (let loop ([pos start] [w 0])
    (cond [(>= pos end) end]
          [(>= pos (gap-length gb)) pos]
          [else
           (let-values ([(ch clen) (gap-char+len gb pos)])
             (define cw (max 0 (char-display-width ch)))
             (if (> (+ w cw) max-width)
                 pos
                 (loop (+ pos clen) (+ w cw))))])))
