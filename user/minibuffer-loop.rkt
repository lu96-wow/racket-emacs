#lang racket

;; user/minibuffer-loop.rkt — Minibuffer read loop

(require "../kernel/buffer.rkt"
         "../core/window.rkt"
         "../kernel/keymap.rkt"
         "../kernel/bottom-input.rkt"
         "../core/minibuffer.rkt"
         "../kernel/kill-ring.rkt"
         "../base/registry.rkt"
         "../platform/event.rkt"
         "../display/render.rkt")

(provide read-from-minibuffer!)

(define (call-with-minibuffer frm prompt #:keymap km #:initial initial proc)
  (define saved-state (activate-minibuffer! frm prompt))
  (bottom-line-activate-input! prompt #:initial initial #:history (minibuffer-history))
  (dynamic-wind void proc
    (λ () (deactivate-minibuffer! frm saved-state) (bottom-line-deactivate-input!))))

(define (read-from-minibuffer! prompt
                              #:keymap [km minibuffer-local-map]
                              #:initial [initial ""])
  (define frm (current-frame))
  (call-with-minibuffer frm prompt #:keymap km #:initial initial
    (λ ()
      (let read-loop ()
        (display-frame frm) (define ke (read-key-event!))
        (unless ke (read-loop))
        (cond [(mouse-event? ke) (read-loop)] [(key-event-cancel? ke) #f]
              [else (define cmd (lookup-key km (list ke)))
               (cond [(eq? cmd 'minibuffer-exit) (define input (bottom-line-get-input))
                      (minibuffer-history-push! input) (minibuffer-history-reset!) input]
                     [(eq? cmd 'minibuffer-abort) #f]
                     [(eq? cmd 'minibuffer-beginning-of-line) (bottom-line-move-beginning!) (read-loop)]
                     [(eq? cmd 'minibuffer-end-of-line) (bottom-line-move-end!) (read-loop)]
                     [(eq? cmd 'minibuffer-backward-char) (bottom-line-move-char! -1) (read-loop)]
                     [(eq? cmd 'minibuffer-forward-char) (bottom-line-move-char! 1) (read-loop)]
                     [(eq? cmd 'minibuffer-delete-backward-char) (bottom-line-delete-backward!) (read-loop)]
                     [(eq? cmd 'minibuffer-delete-char) (bottom-line-delete-forward!) (read-loop)]
                     [(eq? cmd 'minibuffer-kill-line) (bottom-line-kill-line!) (read-loop)]
                     [(eq? cmd 'minibuffer-yank) (define text (current-kill))
                      (when text (bottom-line-insert! text)) (read-loop)]
                     [(eq? cmd 'minibuffer-previous-history) (bottom-line-history-prev!) (read-loop)]
                     [(eq? cmd 'minibuffer-next-history) (bottom-line-history-next!) (read-loop)]
                     [(procedure? cmd) (cmd) (read-loop)]
                     [(key-event-self-insert? ke) (bottom-line-insert! (string (key-event-char ke))) (read-loop)]
                     [else (read-loop)])])))))
