#lang racket

;; input/key.rkt — Key event: three disjoint types
;;
;;   key-char   — printable character
;;   key-ctrl   — control character (Ctrl+A → #\a)
;;   key-sym    — named key (arrows, return, backspace, ...)
;;
;; Each struct only admits valid states.  No null fields, no
;; impossible combinations.  All are #:transparent so they
;; work as hash keys in a keymap.

(provide
 key-char? key-char key-char-ch
 key-ctrl? key-ctrl key-ctrl-ch
 key-sym?  key-sym  key-sym-name
 key-mouse? key-mouse key-mouse-button key-mouse-x key-mouse-y
 key-mouse-action key-mouse-mods

 ;; classification
 key-self-insert?
 key-idle?
 key-quit?)

;; ============================================================
;; Types
;; ============================================================

(struct key-char (ch) #:transparent)
;; ch : char? — the printable character (≥32, not DEL)

(struct key-ctrl (ch) #:transparent)
;; ch : char? — the letter: Ctrl+A → #\a, Ctrl+C → #\c

(struct key-sym (name) #:transparent)
;; name : symbol? — 'up 'down 'left 'right 'return 'backspace 'tab
;;        'delete 'home 'end 'escape 'idle ...

(struct key-mouse (button x y action mods) #:transparent)
;; button : symbol? — 'left 'middle 'right 'wheel-up 'wheel-down
;; x y    : exact-nonnegative-integer? — terminal column/row (0-based)
;; action : symbol? — 'press 'release 'move 'scroll
;; mods   : exact-nonnegative-integer? — modifier bitmask

;; ============================================================
;; Classification
;; ============================================================

(define (key-self-insert? ke)
  (and (key-char? ke)
       (>= (char->integer (key-char-ch ke)) 32)
       (not (char=? (key-char-ch ke) #\rubout))))

(define (key-idle? ke)
  (and (key-sym? ke) (eq? (key-sym-name ke) 'idle)))

(define (key-quit? ke)
  (or (and (key-ctrl? ke) (memv (key-ctrl-ch ke) '(#\c #\d #\g)) #t)
      (and (key-sym? ke) (eq? (key-sym-name ke) 'cancel))))
