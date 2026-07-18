#lang racket

;; edit-debug.rkt — Debug-wrapped edit commands
;;
;; Same signatures as edit.rkt, but each command returns an extra
;; step-snapshot value capturing post-command state.  Usage:
;;
;;   (require (prefix-in debug: "edit-debug.rkt"))
;;   (define-values (db snap) (debug:cmd-self-insert db #\a))
;;
;; Or import bare names when not using edit.rkt simultaneously:
;;   (require "edit-debug.rkt")

(require (prefix-in e: "edit.rkt")
         "kernel/dirty.rkt"
         "kernel/buffer.rkt"
         "kernel/data/text.rkt"
         "kernel/data/face.rkt"
         "kernel/dirty-debug.rkt"
         "kernel/buffer-debug.rkt"
         "kernel/data-debug/gap-debug.rkt"
         "kernel/data-debug/face-debug.rkt"
         "kernel/undo-debug/recorder-debug.rkt")

(provide
 step-snapshot? step-snapshot
 step-snapshot-name
 step-snapshot-dirty step-snapshot-buffer step-snapshot-gap
 step-snapshot-faces step-snapshot-undo

 ;; content-modifying
 cmd-self-insert cmd-newline cmd-tab
 cmd-backward-delete cmd-forward-delete cmd-kill-line
 cmd-yank cmd-undo cmd-redo

 ;; movement
 cmd-forward-char cmd-backward-char
 cmd-beginning-of-line cmd-end-of-line
 cmd-next-line cmd-prev-line

 ;; syntax-driven
 cmd-forward-word cmd-backward-word
 cmd-forward-sexp cmd-backward-sexp
 cmd-kill-word cmd-backward-kill-word

 ;; raw snapshot
 snapshot-from-db)

;; ============================================================
;; Step snapshot
;; ============================================================

(struct step-snapshot
  (name   dirty buffer gap faces undo)
  #:transparent)

(define (snapshot-from-db db name)
  (define buf (dirty-buffer-buf db))
  (define gb  (text-gap (buffer-text buf)))
  (step-snapshot
   name
   (dirty-debug-summary db)
   (buffer-debug-summary buf)
   (gap-debug-summary gb)
   (face-debug-ranges gb)
   (recorder-debug-summary (buffer-undo-recorder buf))))

;; ============================================================
;; Wrappers
;; ============================================================

(define ((wrap-db name fn) db . args)
  (define new-db (apply fn db args))
  (values new-db (snapshot-from-db new-db name)))

(define ((wrap-db-st name fn) db st)
  (define new-db (fn db st))
  (values new-db (snapshot-from-db new-db name)))

;; ============================================================
;; Content-modifying
;; ============================================================

(define (cmd-self-insert db ch)     ((wrap-db 'cmd-self-insert     e:cmd-self-insert)     db ch))
(define cmd-newline                 (wrap-db 'cmd-newline          e:cmd-newline))
(define cmd-tab                     (wrap-db 'cmd-tab              e:cmd-tab))
(define (cmd-backward-delete db)    ((wrap-db 'cmd-backward-delete e:cmd-backward-delete) db))
(define (cmd-forward-delete db)     ((wrap-db 'cmd-forward-delete  e:cmd-forward-delete)  db))
(define (cmd-kill-line db)          ((wrap-db 'cmd-kill-line       e:cmd-kill-line)       db))
(define (cmd-yank db)               ((wrap-db 'cmd-yank            e:cmd-yank)            db))
(define (cmd-undo db)               ((wrap-db 'cmd-undo            e:cmd-undo)            db))
(define (cmd-redo db)               ((wrap-db 'cmd-redo            e:cmd-redo)            db))

;; ============================================================
;; Movement
;; ============================================================

(define (cmd-forward-char db)       ((wrap-db 'cmd-forward-char       e:cmd-forward-char)       db))
(define (cmd-backward-char db)      ((wrap-db 'cmd-backward-char      e:cmd-backward-char)      db))
(define (cmd-beginning-of-line db)  ((wrap-db 'cmd-beginning-of-line  e:cmd-beginning-of-line)  db))
(define (cmd-end-of-line db)        ((wrap-db 'cmd-end-of-line        e:cmd-end-of-line)        db))
(define (cmd-next-line db)          ((wrap-db 'cmd-next-line          e:cmd-next-line)          db))
(define (cmd-prev-line db)          ((wrap-db 'cmd-prev-line          e:cmd-prev-line)          db))

;; ============================================================
;; Syntax-driven
;; ============================================================

(define (cmd-forward-word db st)        ((wrap-db-st 'cmd-forward-word        e:cmd-forward-word)        db st))
(define (cmd-backward-word db st)       ((wrap-db-st 'cmd-backward-word       e:cmd-backward-word)       db st))
(define (cmd-forward-sexp db st)        ((wrap-db-st 'cmd-forward-sexp        e:cmd-forward-sexp)        db st))
(define (cmd-backward-sexp db st)       ((wrap-db-st 'cmd-backward-sexp       e:cmd-backward-sexp)       db st))
(define (cmd-kill-word db st)           ((wrap-db-st 'cmd-kill-word           e:cmd-kill-word)           db st))
(define (cmd-backward-kill-word db st)  ((wrap-db-st 'cmd-backward-kill-word  e:cmd-backward-kill-word)  db st))
