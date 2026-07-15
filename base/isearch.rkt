#lang racket

;; base/isearch.rkt — Incremental search (built on kernel primitives)

(require "../kernel/buffer.rkt"
         "../kernel/gap.rkt"
         "../kernel/key-event.rkt"
         "../kernel/event-chain.rkt"
         "../kernel/bottom-input.rkt"
         "../core/search.rkt")

(provide isearch-forward isearch-backward isearch-active?)

(define isearch-state (box #f))
(define (isearch-active?) (vector? (unbox isearch-state)))

(define (isearch-prompt str dir) (string-append (if (= dir 1) "I-search: " "I-search backward: ") str))
(define (isearch-fail str dir) (string-append (if (= dir 1) "failing I-search: " "failing I-search backward: ") str))

(define (make-isearch-handler init-dir)
  (define buf (current-buffer)) (define gb (buffer-gap buf))
  (define start-pt (buffer-point buf)) (define search-str "") (define direction init-dir)
  (define match-start start-pt) (define match-end start-pt)
  (bottom-line-set-echo! (isearch-prompt "" init-dir))
  (λ (ke)
    (cond [(key-event-cancel? ke) (set-buffer-point! buf start-pt) (clear-isearch!) #f]
          [(key-event-self-insert? ke)
           (set! search-str (string-append search-str (string (key-event-char ke))))
           (define search-from (if (= direction 1) match-end (max 0 (sub1 match-start))))
           (define-values (ms me) (if (= direction 1) (search-fwd gb search-str search-from) (search-bwd gb search-str search-from)))
           (if ms (begin (set! match-start ms) (set! match-end me) (set-buffer-point! buf ms)
                         (bottom-line-set-echo! (isearch-prompt search-str direction)))
               (bottom-line-set-echo! (isearch-fail search-str direction))) #f]
          [(key-event-backspace? ke)
           (when (positive? (string-length search-str))
             (set! search-str (substring search-str 0 (sub1 (string-length search-str))))
             (if (positive? (string-length search-str))
                 (let ([search-from (if (= direction 1) (max 0 (buffer-point buf)) (max 0 (sub1 (buffer-point buf))))])
                   (define-values (ms me) (if (= direction 1) (search-fwd gb search-str search-from) (search-bwd gb search-str search-from)))
                   (if ms (begin (set! match-start ms) (set! match-end me) (set-buffer-point! buf ms)
                                 (bottom-line-set-echo! (isearch-prompt search-str direction)))
                       (bottom-line-set-echo! (isearch-fail search-str direction))))
                 (begin (set-buffer-point! buf start-pt) (clear-isearch!)))) #f]
          [(and (key-event-ctrl? ke) (key-event-char ke) (memv (char-downcase (key-event-char ke)) '(#\s #\r)))
           (set! direction (if (char-ci=? (key-event-char ke) #\s) 1 -1))
           (when (positive? (string-length search-str))
             (define search-from (if (= direction 1) match-end (max 0 (sub1 match-start))))
             (define-values (ms me) (if (= direction 1) (search-fwd gb search-str search-from) (search-bwd gb search-str search-from)))
             (when ms (set! match-start ms) (set! match-end me) (set-buffer-point! buf ms)))
           (bottom-line-set-echo! (isearch-prompt search-str direction)) #f]
          [else (set-buffer-point! buf match-start) (clear-isearch!) ke])))

(define (clear-isearch!) (set-box! isearch-state #f) (bottom-line-clear-echo!) (pop-event-handler!))
(define (isearch-forward) (push-event-handler! (make-isearch-handler 1)))
(define (isearch-backward) (push-event-handler! (make-isearch-handler -1)))
