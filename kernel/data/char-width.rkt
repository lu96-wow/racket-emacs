#lang racket

;; kernel/data/char-width.rkt — Character display width calculation
;;
;; Pure functions.  Classifies each codepoint into 0, 1, or 2 display
;; columns.  Used by layout.rkt to compute screen positions.
;;
;; Dependencies: kernel/data/gap.rkt, kernel/data/query.rkt (pure queries only)

(require "gap.rkt"
         "query.rkt")

(provide
 char-display-width
 gap-display-width
 scan-display-width
 tab-width
 cjk-ambiguous-width)  ; 终端 CJK 列宽配置

;; ============================================================
;; tab-width — parameterisable
;; ============================================================

(define tab-width (make-parameter 8))

;; cjk-ambiguous-width — Termux 等终端 CJK 只占 1 列时设为 1
;; 标准终端 (qterminal, gnome-terminal, iTerm2) 用默认值 2
(define cjk-ambiguous-width (make-parameter 2))

;; ============================================================
;; char-display-width — terminal column count
;; ============================================================

(define (char-display-width ch)
  (define cp (char->integer ch))
  (cond
    [(or (< cp 32) (= cp #x7F)) -1]
    [(= cp 9) (tab-width)]
    [(zero-width? cp) 0]
    [(wide? cp) (cjk-ambiguous-width)]
    [else 1]))

(define (zero-width? cp)
  (or (<= #x0300 cp #x036F)  (<= #x0483 cp #x0489)
      (<= #x0591 cp #x05BD)  (= cp #x05BF)
      (<= #x05C1 cp #x05C2)  (<= #x05C4 cp #x05C5) (= cp #x05C7)
      (<= #x0610 cp #x061A)  (<= #x064B cp #x065F) (= cp #x0670)
      (= cp #x0711)          (<= #x0730 cp #x074A)
      (<= #x07A6 cp #x07B0)  (<= #x200B cp #x200F)
      (<= #x2028 cp #x202E)  (<= #x2060 cp #x206F)
      (<= #xFE00 cp #xFE0F)  (= cp #xFEFF)
      (<= #xFFF9 cp #xFFFB)  (<= #x1F3FB cp #x1F3FF)
      (<= #xE0020 cp #xE007F) (<= #xE0100 cp #xE01EF)))

(define (wide? cp)
  (or (<= #x1100 cp #x115F)      (<= #x2329 cp #x232A)
      (<= #x2E80 cp #x303E)      (<= #x3041 cp #x33BF)
      (<= #x3400 cp #x4DBF)      (<= #x4E00 cp #xA4CF)
      (<= #xA960 cp #xA97C)      (<= #xAC00 cp #xD7A3)
      (<= #xF900 cp #xFAFF)      (<= #xFE10 cp #xFE19)
      (<= #xFE30 cp #xFE6F)      (<= #xFF01 cp #xFF60)
      (<= #xFFE0 cp #xFFE6)      (>= cp #x1F000)
      (<= #x20000 cp #x2FFFD)    (<= #x30000 cp #x3FFFD)))

;; ============================================================
;; gap-display-width — total display columns of [from, to)
;; ============================================================

(define (gap-display-width gb from to)
  (let loop ([pos from] [w 0])
    (if (>= pos to)
        w
        (let-values ([(ch clen) (gap-char+len gb pos)])
          (loop (+ pos clen) (+ w (max 0 (char-display-width ch))))))))

;; ============================================================
;; scan-display-width — advance at most max-width columns
;; ============================================================

(define (scan-display-width gb start end max-width)
  (let loop ([pos start] [w 0])
    (cond [(>= pos end) pos]
          [(>= pos (gap-length gb)) pos]
          [else
           (let-values ([(ch clen) (gap-char+len gb pos)])
             (define cw (max 0 (char-display-width ch)))
             (if (> (+ w cw) max-width)
                 pos
                 (loop (+ pos clen) (+ w cw))))])))
