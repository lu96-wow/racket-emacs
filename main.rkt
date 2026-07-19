#lang racket

;; main.rkt — Event loop

(require "display-edit.rkt"
         "display/face.rkt"
         "display/window.rkt"
         "platform/ansi.rkt"
         "platform/termios.rkt"
         "kernel/buffer.rkt"
         "kernel/dirty.rkt"
         "kernel/data/text.rkt"
         "kernel/data/syntax.rkt"
         "kernel/bracket-colorer.rkt"
         "edit.rkt"
         "input/key.rkt"
         "input/parse.rkt"
         "input/keymap.rkt")

(define initial-content
  (string-append
   ";; racket-emacs-rebuild\n"
   ";; C-f/b C-n/p C-a/e C-k C-y C-w C-t C-z/x C-c\n"))

(define (edit db frm fn)  (values (fn db) frm #t))
(define (move db frm fn)  (values (fn db) frm #f))
(define (window db frm fn) (values db (fn frm) #t))

(define global-keymap
  (make-keymap
   (cons (key-sym 'up)        (edit-cmd cmd-prev-line))
   (cons (key-sym 'down)      (edit-cmd cmd-next-line))
   (cons (key-sym 'left)      (edit-cmd cmd-backward-char))
   (cons (key-sym 'right)     (edit-cmd cmd-forward-char))
   (cons (key-sym 'home)      (edit-cmd cmd-beginning-of-line))
   (cons (key-sym 'end)       (edit-cmd cmd-end-of-line))
   (cons (key-sym 'backspace) (edit-cmd cmd-backward-delete))
   (cons (key-sym 'delete)    (edit-cmd cmd-forward-delete))
   (cons (key-sym 'return)    (edit-cmd cmd-newline))
   (cons (key-sym 'tab)       (edit-cmd cmd-tab))
   (cons (key-sym 'escape)    nop-cmd)
   (cons (key-ctrl #\a) (edit-cmd cmd-beginning-of-line))
   (cons (key-ctrl #\e) (edit-cmd cmd-end-of-line))
   (cons (key-ctrl #\f) (edit-cmd cmd-forward-char))
   (cons (key-ctrl #\b) (edit-cmd cmd-backward-char))
   (cons (key-ctrl #\p) (edit-cmd cmd-prev-line))
   (cons (key-ctrl #\n) (edit-cmd cmd-next-line))
   (cons (key-ctrl #\w) (edit-cmd cmd-kill-region))
   (cons (key-ctrl #\t) (edit-cmd cmd-set-mark))
   (cons (key-ctrl #\u) (edit-cmd cmd-forward-delete))
   (cons (key-ctrl #\k) (edit-cmd cmd-kill-line))
   (cons (key-ctrl #\y) (edit-cmd cmd-yank))
   (cons (key-ctrl #\z) (edit-cmd cmd-undo))
   (cons (key-ctrl #\x) (edit-cmd cmd-redo))
   (cons (key-ctrl #\v) (window-cmd (λ (f) (frame-split-leaf! f 'vertical) f)))
   (cons (key-ctrl #\o) (window-cmd (λ (f) (frame-select-next! f) f)))
   (cons (key-sym 'resize) (window-cmd (λ (f)
                             (detect-terminal-size!)
                             (frame-resize f (terminal-width) (terminal-height)))))
   (cons (cons 'left 'press)
         (mouse-cmd (λ (db f ke)
           (define a? (frame-point-to-xy! f (key-mouse-x ke) (key-mouse-y ke)))
           (values db f a?))))))

(define (run)
  (dynamic-wind void
    (λ ()
      (screen-init!)
      (detect-terminal-size!)
      (format-alt-screen-enable)
      (display format-mouse-enable)
      (display format-clear-screen)
      (flush-output)
      (define buf (make-buffer "*scratch*" initial-content))
      (define db  (make-dirty-buffer buf))
      (init-face-cache!)
      (define fc (current-face-cache))
      (bracket-register-faces!)
      (define racket-st (make-racket-syntax-table))
      (define bkt (make-bracket-colorer fc))
      ;; Initial full bracket scan
      (bracket-colorer-rescan-all! bkt (text-gap (buffer-text buf)) racket-st)
      (set-buffer-point! buf (buffer-length buf))
      (define frm (make-frame buf (terminal-width) (terminal-height)))
      (define caches (make-hasheq))

      (define cache-vb (redisplay-init! frm fc caches))

      (let loop ([db db] [frm frm] [cache-vb cache-vb] [caches caches]
                 [bkt bkt])
        (define ke (read-key))
        (cond
          [(key-quit? ke) (void)]
          [(key-idle? ke) (loop db frm cache-vb caches bkt)]
          [(and (key-sym? ke) (eq? (key-sym-name ke) 'resize))
           (detect-terminal-size!)
           (define f (frame-resize frm (terminal-width) (terminal-height)))
           (define-values (d _ vb cs)
             (redisplay! db f fc caches cache-vb
                         #:frame-changed? #t
                         #:bracket-colorer bkt
                         #:syntax-table racket-st))
           (loop d f vb cs bkt)]
          [else
           (define-values (d f a?) (dispatch-key global-keymap db frm ke cmd-self-insert))
           (define-values (db2 frm2 vb2 cs2)
             (redisplay! d f fc caches cache-vb
                         #:content-changed? a?
                         #:frame-changed? (and a? (not (eq? frm f)))
                         #:bracket-colorer bkt
                         #:syntax-table racket-st))
           (loop db2 frm2 vb2 cs2 bkt)])))
    (λ ()
      (display format-cursor-show)
      (display format-reset)
      (format-alt-screen-disable)
      (screen-cleanup!)
      (exit 0))))

(module+ main (run))
