#lang racket

;; api/command.rkt — Command protocol with two-level undo
;;
;; Two levels of undo:
;;   Command-level — reverses any command (nav/edit/window)
;;   Buffer-level  — reverses text changes only (buffer-undo!/redo!)
;;
;; A command carries:
;;   fn       — execute: (window frame key-event) -> void
;;   undo-fn  — (window frame state) -> void    (how to reverse)
;;   state-fn — (window frame) -> state         (what to snapshot)
;;
;; The event-loop snapshots state before execution, pushes onto
;; the command history, and cmd-undo pops + calls undo-fn.
;;
;; Macros handle the common patterns:
;;   define-command            — navigation: auto-capture point, undo restores it
;;   define-modify-command     — editing: undo delegates to buffer-undo!
;;   define-command #:undo ... — explicit undo (window ops)

(provide
 command? command
 command-name command-fn command-undo-fn command-state-fn

 command-history command-history-push! command-history-pop!
 command-redo-push! command-redo-pop!

 define-no-undo-command)

;; ============================================================
;; Struct
;; ============================================================

(struct command
  (name       ; string
   fn         ; (window frame key-event) -> void
   undo-fn    ; (or/c #f (window frame state) -> void)
   state-fn)  ; (or/c #f (window frame) -> state)
  #:transparent)

;; ============================================================
;; Command history — global undo chain
;; ============================================================

;; Each entry: (cons command state)
;; state is the value returned by state-fn before execution.
(define command-history (box '()))
(define command-redo    (box '()))

(define (command-history-push! cmd state)
  (set-box! command-history (cons (cons cmd state) (unbox command-history)))
  ;; Clear redo stack on new command
  (set-box! command-redo '()))

(define (command-history-pop!)
  (define hist (unbox command-history))
  (if (pair? hist)
      (begin
        (set-box! command-history (cdr hist))
        (car hist))
      #f))

(define (command-redo-push! cmd state)
  (set-box! command-redo (cons (cons cmd state) (unbox command-redo))))

(define (command-redo-pop!)
  (define redos (unbox command-redo))
  (if (pair? redos)
      (begin
        (set-box! command-redo (cdr redos))
        (car redos))
      #f))

;; ============================================================
;; define-no-undo-command — toggle, scroll, split, etc.
;; ============================================================

(define-syntax-rule (define-no-undo-command id name (win frm evt) body ...)
  (define id
    (command name (λ (win frm evt) body ...) #f #f)))
