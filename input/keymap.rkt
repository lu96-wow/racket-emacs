#lang racket

;; input/keymap.rkt — Keymap: hash(key-event → command) + dispatch
;;
;; ============================================================================
;; A keymap is an immutable hash mapping any key-char/key-ctrl/key-sym/mouse
;; to a command procedure with uniform signature:
;;
;;   (db frm [ke]) → (values db frm acted?)
;;
;; ============================================================================
;; Computation vs Application
;; ============================================================================
;;
;;   ── Pure (data construction + lookup) ──
;;     make-keymap        bindings → keymap
;;     keymap-lookup      keymap ke → procedure? | #f
;;     keymap-merge       base local → merged keymap
;;     keymap-resolve     local fallback → effective keymap
;;
;;   ── Application (dispatch with side effects on db/frm) ──
;;     dispatch-key       keymap db frm ke fallback-fn → (values db frm acted?)
;;
;; ============================================================================
;; Command Helpers
;; ============================================================================
;;
;;   edit-cmd       (db→db fn) → (db frm → db frm #t)
;;   window-cmd     (frm→frm fn) → (db frm → db frm #t)
;;   mouse-cmd      (db frm ke → db frm) → (db frm ke → db frm acted?)
;;   quit-cmd       → (db frm → db frm 'quit)
;;   nop-cmd        → (db frm → db frm #f)
;;
;; ============================================================================
;; Dependencies: input/key.rkt
;; ============================================================================

(require "key.rkt")

(provide
 ;; ── keymap ──
 keymap? keymap keymap-table
 make-keymap
 keymap-lookup

 ;; ── keymap composition (pure) ──
 keymap-merge
 keymap-resolve

 ;; ── dispatch ──
 dispatch-key

 ;; ── command helpers ──
 edit-cmd
 window-cmd
 mouse-cmd
 quit-cmd
 nop-cmd)

;; ============================================================
;; Keymap — immutable pure data
;; ============================================================

(struct keymap (table) #:transparent)
;; table : (immutable-hash/c key-event procedure?)

(define (make-keymap . bindings)
  ;; bindings : (listof (cons/c key-event procedure?))
  ;; Build an immutable keymap from a list of (key . cmd) pairs.
  ;; Pure — no mutation, no side effects.
  (keymap (for/fold ([h (hash)]) ([p (in-list bindings)])
            (hash-set h (car p) (cdr p)))))

(define (keymap-lookup km ke)
  ;; Look up a key event in the keymap.
  ;; Returns the command procedure, or #f if not found.
  (hash-ref (keymap-table km) ke (λ () #f)))

;; ============================================================
;; keymap-merge — pure composition
;; ============================================================

(define (keymap-merge base local)
  ;; base  : keymap  — fallback (e.g. global keymap)
  ;; local : keymap  — overrides (e.g. buffer-local keymap)
  ;; → keymap — merged; local bindings override base.
  ;; Pure — returns a new keymap.
  (keymap (for/fold ([h (keymap-table base)])
                    ([(k v) (in-hash (keymap-table local))])
            (hash-set h k v))))

;; ============================================================
;; keymap-resolve — choose effective keymap
;; ============================================================

(define (keymap-resolve local fallback)
  ;; local    : keymap | #f — per-buffer keymap
  ;; fallback : keymap      — global keymap
  ;; → keymap — (merge fallback local) if local exists, else fallback
  (if local (keymap-merge fallback local) fallback))

;; ============================================================
;; Command helpers
;; ============================================================

(define (edit-cmd fn)
  ;; fn : dirty-buffer? → dirty-buffer?
  ;; Returns: db frm → (values db frm #t)
  (λ (db frm) (values (fn db) frm #t)))

(define (window-cmd fn)
  ;; fn : frame? → frame?
  ;; Returns: db frm → (values db frm #t)
  (λ (db frm) (values db (fn frm) #t)))

(define (mouse-cmd fn)
  ;; fn : db × frm × key-mouse → (values db frm acted?)
  ;; The handler receives the full mouse event with coordinates.
  (λ (db frm ke) (fn db frm ke)))

(define quit-cmd
  (λ (db frm) (values db frm 'quit)))

(define nop-cmd
  (λ (db frm) (values db frm #f)))

;; ============================================================
;; dispatch-key — lookup → self-insert → ignore
;; ============================================================

(define (dispatch-key km db frm ke self-insert-fn)
  ;; km   : keymap?
  ;; db   : dirty-buffer?
  ;; frm  : frame?
  ;; ke   : key-char | key-ctrl | key-sym | key-mouse
  ;; self-insert-fn : db char → db
  ;;
  ;; Dispatch order:
  ;;   1. Mouse events: match by (button . action) pair
  ;;   2. Keymap lookup: exact key match
  ;;   3. Self-insert: printable character → self-insert-fn
  ;;   4. Otherwise: ignore (nop)
  (cond
    ;; Mouse events: match by (button . action) pair, not full struct.
    ;; Mouse coordinates are variable — button+action is the stable identity.
    ;; The handler receives the original key-mouse struct with x,y.
    [(key-mouse? ke)
     (define mouse-key (cons (key-mouse-button ke) (key-mouse-action ke)))
     (cond
       [(keymap-lookup km mouse-key)
        => (λ (cmd) (cmd db frm ke))]
       [else (values db frm #f)])]

    ;; Exact key match
    [(keymap-lookup km ke)
     => (λ (cmd) (cmd db frm))]

    ;; Self-insert: printable character
    [(key-self-insert? ke)
     (values (self-insert-fn db (key-char-ch ke)) frm #t)]

    ;; Unknown/ignored
    [else (values db frm #f)]))
