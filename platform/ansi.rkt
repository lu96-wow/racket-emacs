#lang racket

;; platform/ansi.rkt — ANSI escape sequence constants
;;
;; ============================================================================
;; Computation Only — pure format strings, zero internal dependencies.
;; All terminal control sequences used by the editor.
;; ============================================================================

(provide
 ;; ── cursor ──
 format-cursor-move format-cursor-hide format-cursor-show
 format-cursor-save format-cursor-restore

 ;; ── screen ──
 format-clear-screen format-clear-to-eol
 format-alt-screen-enable format-alt-screen-disable

 ;; ── bracketed paste ──
 format-bracketed-paste-enable format-bracketed-paste-disable

 ;; ── attributes ──
 format-reset format-bold format-dim format-italic
 format-underline format-blink format-reverse

 ;; ── mouse ──
 format-mouse-enable format-mouse-disable

 ;; ── color (24-bit truecolor + 256 + 16) ──
 ansi-fg ansi-bg
 ansi-fg-256 ansi-bg-256
 ansi-fg-16 ansi-bg-16

 ;; ── color capability detection ──
 detect-color-depth! color-depth)

;; ============================================================
;; Cursor
;; ============================================================

(define (format-cursor-move row col)
  ;; row, col are 0-based — ANSI is 1-based, so add 1.
  (unless (and (exact-nonnegative-integer? row) (exact-nonnegative-integer? col))
    (raise-argument-error 'format-cursor-move
                          "non-negative row and col" (list row col)))
  (format "\e[~a;~aH" (add1 row) (add1 col)))

(define format-cursor-hide    "\e[?25l")
(define format-cursor-show    "\e[?25h")
(define format-cursor-save    "\e7")
(define format-cursor-restore "\e8")

;; ============================================================
;; Screen
;; ============================================================

(define format-clear-screen "\e[2J")
(define format-clear-to-eol "\e[K")

(define (format-alt-screen-enable)  (display "\e[?1049h"))
(define (format-alt-screen-disable) (display "\e[?1049l"))

;; ============================================================
;; Text attributes
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

(define format-mouse-enable  "\e[?1000h\e[?1002h\e[?1006h")
(define format-mouse-disable "\e[?1006l\e[?1002l\e[?1000l")

;; ============================================================
;; Bracketed paste
;; ============================================================

(define format-bracketed-paste-enable  "\e[?2004h")
(define format-bracketed-paste-disable "\e[?2004l")

;; ============================================================
;; Color — 24-bit truecolor
;; ============================================================

(define (ansi-fg r g b)
  (unless (and (exact-integer? r) (<= 0 r 255)
               (exact-integer? g) (<= 0 g 255)
               (exact-integer? b) (<= 0 b 255))
    (raise-argument-error 'ansi-fg "RGB integers [0,255]" (list r g b)))
  (format "\e[38;2;~a;~a;~am" r g b))

(define (ansi-bg r g b)
  (unless (and (exact-integer? r) (<= 0 r 255)
               (exact-integer? g) (<= 0 g 255)
               (exact-integer? b) (<= 0 b 255))
    (raise-argument-error 'ansi-bg "RGB integers [0,255]" (list r g b)))
  (format "\e[48;2;~a;~a;~am" r g b))

;; ============================================================
;; Color — 256-color
;; ============================================================

(define (ansi-fg-256 n)
  (unless (and (exact-integer? n) (<= 0 n 255))
    (raise-argument-error 'ansi-fg-256 "integer [0,255]" n))
  (format "\e[38;5;~am" n))

(define (ansi-bg-256 n)
  (unless (and (exact-integer? n) (<= 0 n 255))
    (raise-argument-error 'ansi-bg-256 "integer [0,255]" n))
  (format "\e[48;5;~am" n))

;; ============================================================
;; Color — 16-color
;; ============================================================

(define ansi-16-fg-colors
  '#(30 34 32 36 31 35 33 37 90 94 92 96 91 95 93 97))
(define ansi-16-bg-colors
  '#(40 44 42 46 41 45 43 47 100 104 102 106 101 105 103 107))

(define (ansi-fg-16 n)
  (unless (and (exact-integer? n) (<= 0 n 15))
    (raise-argument-error 'ansi-fg-16 "integer [0,15]" n))
  (format "\e[~am" (vector-ref ansi-16-fg-colors n)))

(define (ansi-bg-16 n)
  (unless (and (exact-integer? n) (<= 0 n 15))
    (raise-argument-error 'ansi-bg-16 "integer [0,15]" n))
  (format "\e[~am" (vector-ref ansi-16-bg-colors n)))

;; ============================================================
;; Color depth detection
;; ============================================================

(define color-depth (make-parameter 'truecolor))

(define (detect-color-depth!)
  (define colorterm (getenv "COLORTERM"))
  (define term (getenv "TERM"))
  (cond
    [(and colorterm (string-ci=? colorterm "truecolor")) (color-depth 'truecolor)]
    [(and term (or (string-contains? term "256color")
                   (string-contains? term "256")))        (color-depth '256)]
    [(and term (string-contains? term "color"))           (color-depth '16)]
    [else                                                 (color-depth 'none)]))
