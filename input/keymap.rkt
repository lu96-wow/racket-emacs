#lang racket

;; input/keymap.rkt — Keymap: hash(key-event → command) + dispatch
;;
;; A keymap is a plain hash mapping any key-char/key-ctrl/key-sym
;; to a command function (db frm → (values db frm acted?)).
;;
;; dispatch-key does three things, in order:
;;   1. lookup the key in the keymap → call command if found
;;   2. key-self-insert? → self-insert (caller provides fallback)
;;   3. otherwise → ignore
;;
;; No coupling to specific key bindings — those are data, defined
;; by the caller (main.rkt or a bindings module).

(require "key.rkt")

(provide
 ;; keymap — immutable pure data
 keymap? keymap keymap-table
 make-keymap
 keymap-lookup

 ;; keymap composition (pure)
 keymap-merge        ;; base local → merged
 keymap-resolve      ;; local fallback → effective keymap

 ;; dispatch
 dispatch-key

 ;; command helpers
 edit-cmd       ;; (db→db fn) → (db frm → db frm)
 window-cmd     ;; (frm→frm fn) → (db frm → db frm)
 mouse-cmd      ;; (db frm ke → db frm) — receives key-mouse struct
 quit-cmd
 nop-cmd)

;; ============================================================
;; Keymap — immutable pure data (analogous to lang-def)
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
  (hash-ref (keymap-table km) ke (λ () #f)))

;; ============================================================
;; keymap-merge — pure composition (analogous to lang-def->syntax-config)
;; ============================================================

(define (keymap-merge base local)
  ;; base  : keymap — fallback (e.g. global keymap)
  ;; local : keymap — overrides (e.g. buffer-local keymap)
  ;; → keymap — merged; local wins on conflict
  ;; Pure — returns a new keymap, neither argument is mutated.
  (keymap (for/fold ([h (keymap-table base)])
                    ([(k v) (in-hash (keymap-table local))])
            (hash-set h k v))))

;; ============================================================
;; keymap-resolve — pure lookup in caller-owned table
;; ============================================================

(define (keymap-resolve local fallback)
  ;; local    : keymap | #f — caller gets it from wherever (struct field, etc.)
  ;; fallback : keymap — default when local is #f
  ;; → keymap — (merge fallback local) if local exists, else fallback
  ;; Pure — no table, no indirection.
  (if local (keymap-merge fallback local) fallback))

;; ============================================================
;; Command helpers — wrap kernel/base-edit + display/window into
;; uniform signature: db frm → (values db frm acted?)
;; ============================================================

(define (edit-cmd fn)
  ;; fn : db → db
  (λ (db frm) (values (fn db) frm #t)))

(define (edit/c-cmd fn)
  ;; fn : db char → db  (for self-insert)
  (λ (db frm ch) (values (fn db ch) frm #t)))

(define (window-cmd fn)
  ;; fn : frm → frm
  (λ (db frm) (values db (fn frm) #t)))

(define (mouse-cmd fn)
  ;; fn : db × frm × key-mouse → (values db frm acted?)
  ;; The function receives the full mouse event struct including
  ;; terminal (x,y) coordinates.  It is responsible for converting
  ;; to buffer positions via frame/window primitives.
  (λ (db frm ke) (fn db frm ke)))

(define quit-cmd
  ;; Returns 'quit as acted? value
  (λ (db frm) (values db frm 'quit)))

(define nop-cmd
  (λ (db frm) (values db frm #f)))

;; ============================================================
;; dispatch-key — lookup → self-insert → ignore
;; ============================================================

(define (dispatch-key km db frm ke self-insert-fn)
  ;; km   : keymap (hash)
  ;; db   : dirty-buffer
  ;; frm  : frame
  ;; ke   : key-char | key-ctrl | key-sym | key-mouse
  ;; self-insert-fn : db char → db  (from kernel/base-edit.rkt)
  (cond
    ;; Mouse events: match by (button . action) pair, not full struct.
    ;; Mouse coordinates are variable — button+action is the stable identity.
    ;; The handler receives the original key-mouse struct with x,y.
    [(key-mouse? ke)
     (define mouse-key (cons (key-mouse-button ke) (key-mouse-action ke)))
     (cond
       [(keymap-lookup km mouse-key) => (λ (cmd) (cmd db frm ke))]
       [else (values db frm #f)])]
    [(keymap-lookup km ke) => (λ (cmd) (cmd db frm))]
    [(key-self-insert? ke)
     (values (self-insert-fn db (key-char-ch ke)) frm #t)]
    [else (values db frm #f)]))
