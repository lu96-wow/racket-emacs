#lang racket

;; input/key.rkt — Key event: four disjoint types with validation
;;
;; ============================================================================
;; Pure data — key-char, key-ctrl, key-sym, key-mouse.
;; All are #:transparent so they work as hash keys in a keymap.
;; No dependencies on any other module.
;; ============================================================================

(provide
 ;; ── constructors + predicates ──
 key-char? key-char key-char-ch
 key-ctrl? key-ctrl key-ctrl-ch
 key-sym?  key-sym  key-sym-name
 key-mouse? key-mouse key-mouse-button key-mouse-x key-mouse-y
 key-mouse-action key-mouse-mods
 key-paste? key-paste key-paste-text

 ;; ── classification ──
 key-self-insert?
 key-idle?
 key-quit?)

;; ============================================================
;; Types
;; ============================================================

(struct key-char (ch) #:transparent)
;; ch : char? — the printable character (codepoint ≥ 32, not DEL)

(struct key-ctrl (ch) #:transparent)
;; ch : char? — the letter: Ctrl+A → #\a, Ctrl+C → #\c
;; Contract: ch is a lowercase letter a–z

(struct key-sym (name) #:transparent)
;; name : symbol? — 'up 'down 'left 'right 'return 'backspace 'tab
;;        'delete 'home 'end 'pageup 'pagedown 'insert
;;        'escape 'idle 'resize 'unknown 'cancel

(struct key-mouse (button x y action mods) #:transparent)
;; button : symbol? — 'left 'middle 'right 'wheel-up 'wheel-down
;; x y    : exact-nonnegative-integer? — terminal column/row (1-based SGR)
;;          NOTE: SGR mouse sends 1-based coordinates.
;;          Convert to 0-based via (sub1 x) (sub1 y) for internal use.
;; action : symbol? — 'press 'release 'move 'scroll
;; mods   : exact-nonnegative-integer? — modifier bitmask

(struct key-paste (text) #:transparent)
;; text : string? — pasted content from bracketed-paste mode

;; ============================================================
;; Classification
;; ============================================================

(define (key-self-insert? ke)
  ;; Is this a printable character that should be inserted as text?
  (and (key-char? ke)
       (>= (char->integer (key-char-ch ke)) 32)
       (not (char=? (key-char-ch ke) #\rubout))))

(define (key-idle? ke)
  ;; No input available — editor can do background work.
  (and (key-sym? ke) (eq? (key-sym-name ke) 'idle)))

(define (key-quit? ke)
  ;; Should the editor exit?  Ctrl-C, Ctrl-D, Ctrl-G, or cancel.
  (or (and (key-ctrl? ke)
           (memv (key-ctrl-ch ke) '(#\c #\d #\g))
           #t)
      (and (key-sym? ke) (eq? (key-sym-name ke) 'cancel))))
