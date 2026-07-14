#lang racket

;; kernel/minibuffer.rkt — Minibuffer window lifecycle + shared history

(require "window.rkt"
         "keymap.rkt"
         "buffer.rkt")

(provide
 minibuffer-local-map
 activate-minibuffer! deactivate-minibuffer! minibuffer-active?
 minibuffer-history minibuffer-history-push! minibuffer-history-reset!)

(define minibuffer-local-map (make-keymap))
(define minibuffer-history (make-parameter '()))
(define history-pos (box -1))

(define (minibuffer-history-push! input)
  (unless (equal? input "") (minibuffer-history (cons input (minibuffer-history)))))
(define (minibuffer-history-reset!) (set-box! history-pos -1))

(struct minibuffer-meta ([outer-window #:mutable] [outer-buf #:mutable]) #:transparent)
(define meta-table (make-hasheq))
(define (minibuffer-active? [frm (current-frame)]) (and (hash-ref meta-table frm (λ () #f)) #t))

(define (activate-minibuffer! frm prompt)
  (define mini (frame-minibuffer-window frm))
  (define outer-win (frame-selected-window frm)) (define outer-buf (and outer-win (window-buffer outer-win)))
  (define m (minibuffer-meta outer-win outer-buf)) (hash-set! meta-table frm m)
  (when outer-win (set-window-selected?! outer-win #f))
  (set-window-selected?! mini #t) (set-frame-selected-window! frm mini) (list outer-win outer-buf))

(define (deactivate-minibuffer! frm state)
  (match-define (list outer-win outer-buf) state) (define mini (frame-minibuffer-window frm))
  (when mini (set-window-selected?! mini #f))
  (when outer-win (set-window-selected?! outer-win #t) (set-frame-selected-window! frm outer-win)
        (when outer-buf (set-buffer outer-buf))) (hash-remove! meta-table frm))
