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

 ;; per-buffer keymap storage (analogous to lang/apply's config-table)
 buffer-keymap-get   ;; buf → keymap | #f
 buffer-keymap-set!  ;; buf keymap → void
 buffer-keymap-resolve ;; buf global-km → effective keymap

 ;; dispatch
 dispatch-key

 ;; command helpers
 edit-cmd       ;; (db→db fn) → (db frm → db frm)
 window-cmd     ;; (frm→frm fn) → (db frm → db frm)
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
;; Per-buffer keymap storage (analogous to lang/apply's config-table)
;; ============================================================

(define buffer-keymap-table (make-hasheq))

(define (buffer-keymap-get buf)
  (hash-ref buffer-keymap-table buf (λ () #f)))

(define (buffer-keymap-set! buf km)
  (hash-set! buffer-keymap-table buf km))

(define (buffer-keymap-resolve buf global-km)
  ;; buf       : buffer?
  ;; global-km : keymap — fallback when no buffer-local keymap
  ;; → keymap  — global + buffer-local merged, or just global
  (define local (buffer-keymap-get buf))
  (if local (keymap-merge global-km local) global-km))

;; ============================================================
;; Command helpers — wrap kernel/edit + display/window into
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
  ;; ke   : key-char | key-ctrl | key-sym
  ;; self-insert-fn : db char → db  (from kernel/edit.rkt)
  (cond
    [(keymap-lookup km ke) => (λ (cmd) (cmd db frm))]
    [(key-self-insert? ke)
     (values (self-insert-fn db (key-char-ch ke)) frm #t)]
    [else (values db frm #f)]))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit
           "../kernel/buffer.rkt"
           "../kernel/dirty.rkt"
           "../kernel/edit.rkt")

  (test-case "keymap lookup"
    (define called? (box #f))
    (define km (make-keymap
                (cons (key-sym 'up)
                      (λ (db frm) (set-box! called? #t) (values db frm #t)))))
    (define cmd (keymap-lookup km (key-sym 'up)))
    (check-true (procedure? cmd))
    (define buf (make-buffer "*t*" ""))
    (cmd (make-dirty-buffer buf) #f)
    (check-true (unbox called?)))

  (test-case "dispatch: keymap hit"
    (define km (make-keymap
                (cons (key-ctrl #\a)
                      (λ (db frm) (values db frm #t)))))
    (define buf (make-buffer "*t*" "hello"))
    (define db  (make-dirty-buffer buf))
    (define-values (db2 _frm acted?)
      (dispatch-key km db #f (key-ctrl #\a) cmd-self-insert))
    (check-true acted?))

  (test-case "dispatch: self-insert fallback"
    (define km (make-keymap))
    (define buf (make-buffer "*t*" ""))
    (define db  (make-dirty-buffer buf))
    (define-values (db2 _frm acted?)
      (dispatch-key km db #f (key-char #\X) cmd-self-insert))
    (check-true acted?)
    (check-equal? (dirty-string db2) "X"))

  (test-case "dispatch: unknown key ignored"
    (define km (make-keymap))
    (define buf (make-buffer "*t*" ""))
    (define db  (make-dirty-buffer buf))
    (define-values (db2 _frm acted?)
      (dispatch-key km db #f (key-sym 'f12) cmd-self-insert))
    (check-false acted?))

  (test-case "command helpers"
    (define buf (make-buffer "*t*" "abc"))
    (define db  (make-dirty-buffer buf))
    ;; edit-cmd
    (define-values (db2 _frm a?) ((edit-cmd cmd-forward-char) db #f))
    (check-true a?)
    (check-equal? (dirty-point db2) 1)
    ;; nop-cmd
    (define-values (db3 _frm2 a2?) (nop-cmd db2 #f))
    (check-false a2?))

  (test-case "keymap-merge: local overrides global"
    (define global (make-keymap
                    (cons (key-char #\a) (edit-cmd cmd-forward-char))
                    (cons (key-char #\b) (edit-cmd cmd-backward-char))))
    (define local  (make-keymap
                    (cons (key-char #\a) (edit-cmd cmd-backward-delete))))
    (define merged (keymap-merge global local))
    ;; local wins on conflict
    (define cmd-a (keymap-lookup merged (key-char #\a)))
    (check-pred procedure? cmd-a)
    ;; global fallback still present
    (define cmd-b (keymap-lookup merged (key-char #\b)))
    (check-pred procedure? cmd-b)
    ;; key not in either
    (check-false (keymap-lookup merged (key-char #\z))))

  (test-case "buffer-keymap-resolve"
    (define global (make-keymap
                    (cons (key-char #\a) (edit-cmd cmd-forward-char))))
    (define local  (make-keymap
                    (cons (key-char #\x) (edit-cmd cmd-backward-delete))))
    (define buf (make-buffer "*test*" ""))
    ;; No buffer-local → returns global as-is
    (define km1 (buffer-keymap-resolve buf global))
    (check-true (procedure? (keymap-lookup km1 (key-char #\a))))
    (check-false (keymap-lookup km1 (key-char #\x)))
    ;; Set buffer-local → merged (local overrides, global still accessible)
    (buffer-keymap-set! buf local)
    (define km2 (buffer-keymap-resolve buf global))
    (check-true (procedure? (keymap-lookup km2 (key-char #\a))))
    (check-true (procedure? (keymap-lookup km2 (key-char #\x))))))
