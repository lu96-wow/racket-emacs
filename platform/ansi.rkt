#lang racket

;; platform/ansi.rkt — ANSI escape sequence constants
;;
;; All terminal control sequences used by the editor.
;; Zero internal dependencies.

(provide
 format-cursor-move format-cursor-hide format-cursor-show
 format-cursor-save format-cursor-restore
 format-clear-screen format-clear-to-eol
 format-alt-screen-enable format-alt-screen-disable
 format-bracketed-paste-enable format-bracketed-paste-disable
 format-reset format-bold format-dim format-italic
 format-underline format-blink format-reverse
 format-mouse-enable format-mouse-disable
 ansi-fg ansi-bg ansi-fg-256 ansi-bg-256 ansi-fg-16 ansi-bg-16
 detect-color-depth! color-depth)

;; ============================================================
;; Cursor
;; ============================================================

(define (format-cursor-move row col)
  (format "\e[~a;~aH" (add1 row) (add1 col)))

(define format-cursor-hide  "\e[?25l")
(define format-cursor-show  "\e[?25h")
(define format-cursor-save  "\e7")
(define format-cursor-restore "\e8")

;; ============================================================
;; Screen
;; ============================================================

(define format-clear-screen "\e[2J")
(define format-clear-to-eol "\e[K")

(define (format-alt-screen-enable)  (display "\e[?1049h"))
(define (format-alt-screen-disable) (display "\e[?1049l"))

;; ============================================================
;; Attributes
;; ============================================================

(define format-reset     "\e[0m")
(define format-bold      "\e[1m")
(define format-dim       "\e[2m")
(define format-italic    "\e[3m")
(define format-underline "\e[4m")
(define format-blink     "\e[5m")
(define format-reverse   "\e[7m")

;; ============================================================
;; Mouse
;; ============================================================

(define format-mouse-enable
  "\e[?1000h\e[?1002h\e[?1006h")

(define format-mouse-disable
  "\e[?1006l\e[?1002l\e[?1000l")

;; ============================================================
;; Bracketed paste
;; ============================================================

(define format-bracketed-paste-enable  "\e[?2004h")
(define format-bracketed-paste-disable "\e[?2004l")

;; ============================================================
;; Color
;; ============================================================

(define (ansi-fg r g b)   (format "\e[38;2;~a;~a;~am" r g b))
(define (ansi-bg r g b)   (format "\e[48;2;~a;~a;~am" r g b))
(define (ansi-fg-256 n)   (format "\e[38;5;~am" n))
(define (ansi-bg-256 n)   (format "\e[48;5;~am" n))

(define ansi-16-fg '#(30 34 32 36 31 35 33 37 90 94 92 96 91 95 93 97))
(define ansi-16-bg '#(40 44 42 46 41 45 43 47 100 104 102 106 101 105 103 107))

(define (ansi-fg-16 n) (format "\e[~am" (vector-ref ansi-16-fg n)))
(define (ansi-bg-16 n) (format "\e[~am" (vector-ref ansi-16-bg n)))

;; ============================================================
;; Color depth
;; ============================================================

(define color-depth (make-parameter 'truecolor))

(define (detect-color-depth!)
  (define colorterm (getenv "COLORTERM"))
  (define term (getenv "TERM"))
  (cond
    [(and colorterm (string-ci=? colorterm "truecolor"))
     (color-depth 'truecolor)]
    [(and term (or (string-contains? term "256color")
                   (string-contains? term "256")))
     (color-depth '256)]
    [(and term (string-contains? term "color"))
     (color-depth '16)]
    [else (color-depth 'none)]))
