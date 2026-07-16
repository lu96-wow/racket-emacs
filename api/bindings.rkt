#lang racket

;; api/bindings.rkt — Key → command mapping

(require "../kernel/key-event/key-event.rkt"
         "command.rkt"
         "navigation.rkt"
         "editing.rkt"
         "window-ops.rkt")

(provide default-bindings lookup-command)

;; ============================================================
;; Key struct for hash-based dispatch
;; ============================================================

(struct key (type value) #:transparent
  #:methods gen:equal+hash
  [(define (equal-proc a b rec)
     (and (eq? (key-type a) (key-type b))
          (equal? (key-value a) (key-value b))))
   (define (hash-proc a rec)
     (equal-hash-code (cons (key-type a) (key-value a))))
   (define (hash2-proc a rec)
     (equal-secondary-hash-code (cons (key-type a) (key-value a))))])

(define (key-event->key evt)
  (cond [(key-event-symbol evt) (key 'symbol (key-event-symbol evt))]
        [(and (key-event-ctrl? evt) (key-event-char evt))
         (key 'ctrl (char-downcase (key-event-char evt)))]
        [(key-event-char evt) (key 'char (key-event-char evt))]
        [else (key 'symbol 'unknown)]))

;; ============================================================
;; Default bindings
;; ============================================================

(define default-bindings
  (let ([t (make-hash)])
    ;; Navigation
    (hash-set! t (key 'symbol 'up)        cmd-prev-line)
    (hash-set! t (key 'symbol 'down)      cmd-next-line)
    (hash-set! t (key 'symbol 'right)     cmd-forward-char)
    (hash-set! t (key 'symbol 'left)      cmd-backward-char)
    (hash-set! t (key 'symbol 'home)      cmd-beginning-of-line)
    (hash-set! t (key 'symbol 'end)       cmd-end-of-line)
    (hash-set! t (key 'ctrl #\f)          cmd-forward-char)
    (hash-set! t (key 'ctrl #\b)          cmd-backward-char)
    (hash-set! t (key 'ctrl #\a)          cmd-beginning-of-line)
    (hash-set! t (key 'ctrl #\e)          cmd-end-of-line)
    (hash-set! t (key 'ctrl #\p)          cmd-prev-line)
    (hash-set! t (key 'ctrl #\n)          cmd-next-line)
    ;; Editing
    (hash-set! t (key 'symbol 'return)    cmd-newline)
    (hash-set! t (key 'symbol 'tab)       cmd-tab)
    (hash-set! t (key 'symbol 'backspace) cmd-backward-delete)
    (hash-set! t (key 'ctrl #\d)          cmd-forward-delete)
    (hash-set! t (key 'ctrl #\k)          cmd-kill-line)
    (hash-set! t (key 'ctrl #\y)          cmd-yank)
    (hash-set! t (key 'ctrl #\t)          cmd-toggle-wrap-mode)
    ;; Undo/Redo
    (hash-set! t (key 'ctrl #\_)          cmd-undo)
    (hash-set! t (key 'ctrl #\r)          cmd-redo)
    ;; Window
    (hash-set! t (key 'ctrl #\x)          cmd-other-window)
    t))

(define (lookup-command bindings evt)
  (define k (key-event->key evt))
  (hash-ref bindings k (λ () #f)))
