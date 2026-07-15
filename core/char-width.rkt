#lang racket

;; base/char-width.rkt — Terminal character display width (wcwidth)
;;
;; Also provides gap-level display-width scanning.
;; Used by both edit.rkt (current-column) and display/render.rkt (layout).
;; Dependency: base/gap.rkt

(require "../kernel/gap.rkt")

(provide
 char-display-width        ; char → column count (0, 1, 2, or -1 for control)
 gap-display-width         ; gap-buffer [from, to) total display width
 scan-display-width        ; scan forward N display columns → byte-pos
 tab-width)

;; ============================================================
;; char-display-width — console column width of a Unicode char
;; ============================================================

(define (char-display-width ch)
  (define cp (char->integer ch))
  (cond
    [(or (< cp 32) (= cp #x7F)) -1]   ; control chars
    [(= cp 9) (tab-width)]             ; tab
    ;; Zero-width characters
    [(zero-width-codepoint? cp) 0]
    ;; Wide characters (CJK, fullwidth, emoji etc.)
    [(wide-codepoint? cp) 2]
    [else 1]))

(define (zero-width-codepoint? cp)
  ;; Combining characters and format controls
  (or (<= #x0300 cp #x036F)  (<= #x0483 cp #x0489)
      (<= #x0591 cp #x05BD)  (= cp #x05BF)
      (<= #x05C1 cp #x05C2)  (<= #x05C4 cp #x05C5) (= cp #x05C7)
      (<= #x0610 cp #x061A)  (<= #x064B cp #x065F) (= cp #x0670)
      (= cp #x0711)          (<= #x0730 cp #x074A)
      (<= #x07A6 cp #x07B0)  (<= #x200B cp #x200F)
      (<= #x2028 cp #x202E)  (<= #x2060 cp #x206F)
      (<= #xFE00 cp #xFE0F)  (= cp #xFEFF)
      (<= #xFFF9 cp #xFFFB)  (<= #x1F3FB cp #x1F3FF)  ; emoji skin tones
      (<= #xE0020 cp #xE007F) (<= #xE0100 cp #xE01EF)))

(define (wide-codepoint? cp)
  (or (<= #x1100 cp #x115F)   ; Hangul Jamo
      (<= #x2329 cp #x232A)   ; angle brackets
      (<= #x2E80 cp #x303E)   ; CJK Radicals .. CJK Symbols
      (<= #x3041 cp #x33BF)   ; Hiragana .. CJK Compatibility
      (<= #x3400 cp #x4DBF)   ; CJK Extension A
      (<= #x4E00 cp #xA4CF)   ; CJK Unified, Yi
      (<= #xA960 cp #xA97C)   ; Hangul Jamo Extended-A
      (<= #xAC00 cp #xD7A3)   ; Hangul Syllables
      (<= #xF900 cp #xFAFF)   ; CJK Compatibility Ideographs
      (<= #xFE10 cp #xFE19)   ; Vertical forms
      (<= #xFE30 cp #xFE6F)   ; CJK Compatibility Forms
      (<= #xFF01 cp #xFF60)   ; Fullwidth Forms
      (<= #xFFE0 cp #xFFE6)   ; Fullwidth Signs
      (>= cp #x1F000)         ; Emoji & Pictographs
      (<= #x20000 cp #x2FFFD) ; CJK Extension B+
      (<= #x30000 cp #x3FFFD))) ; CJK Extension G+

;; ============================================================
;; gap-display-width — total display columns of [from, to) byte range
;; ============================================================

(define (gap-display-width gb from to)
  (let loop ([pos from] [w 0])
    (if (>= pos to)
        w
        (let-values ([(ch clen) (gap-char-at gb pos)])
          (loop (+ pos clen) (+ w (max 0 (char-display-width ch))))))))

;; ============================================================
;; scan-display-width — scan to given column count
;; ============================================================

(define (scan-display-width gb start end max-width)
  (let loop ([pos start] [w 0])
    (cond [(>= pos end) pos]
          [(>= pos (gap-byte-length gb)) pos]
          [else
           (let-values ([(ch clen) (gap-char-at gb pos)])
             (define cw (max 0 (char-display-width ch)))
             (if (> (+ w cw) max-width)
                 pos
                 (loop (+ pos clen) (+ w cw))))])))

;; ============================================================
;; tab-width
;; ============================================================

(define tab-width (make-parameter 8))
