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
 ;; keymap
 make-keymap
 keymap-set!
 keymap-lookup

 ;; dispatch
 dispatch-key

 ;; command helpers
 edit-cmd       ;; (db→db fn) → (db frm → db frm)
 window-cmd     ;; (frm→frm fn) → (db frm → db frm)
 quit-cmd
 nop-cmd)

;; ============================================================
;; Keymap — plain hash
;; ============================================================

(define (make-keymap) (make-hash))

(define (keymap-set! km ke cmd)
  (hash-set! km ke cmd))

(define (keymap-lookup km ke)
  (hash-ref km ke (λ () #f)))

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
    (define km (make-keymap))
    (define called? (box #f))
    (keymap-set! km (key-sym 'up)
      (λ (db frm) (set-box! called? #t) (values db frm #t)))
    (define cmd (keymap-lookup km (key-sym 'up)))
    (check-true (procedure? cmd))
    (define buf (make-buffer "*t*" ""))
    (cmd (make-dirty-buffer buf) #f)
    (check-true (unbox called?)))

  (test-case "dispatch: keymap hit"
    (define km (make-keymap))
    (keymap-set! km (key-ctrl #\a)
      (λ (db frm) (values db frm #t)))
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
    (check-false a2?)))
