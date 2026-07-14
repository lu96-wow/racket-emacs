#lang racket

;; user/mode.rkt — Mode abstraction: define, register, switch

(require "../kernel/buffer.rkt"
         "../kernel/keymap.rkt"
         "../kernel/syntax.rkt"
         "font-lock-activate.rkt")

(provide
 define-mode
 mode? mode-name mode-keymap mode-syntax mode-highlight-kw mode-highlight-syntax?
 mode-activate
 set-buffer-mode!
 mode-for-path
 init-all-mode-file-types!)

(struct mode
  (name keymap syntax highlight-kw highlight-syntax? activate) #:transparent)

(define mode-registry (make-hash))
(define mode-file-types (make-hash))

(define (define-mode name
                     #:keymap [km #f]
                     #:syntax [st #f]
                     #:highlight-kw [kw '()]
                     #:highlight-syntax? [hs? #f]
                     #:activate [afn #f]
                     #:file-types [fts '()])
  (define m (mode name km st kw hs? afn))
  (hash-set! mode-registry name m)
  (hash-set! mode-file-types name fts)
  m)

(define (set-buffer-mode! buf target-mode)
  (define m (hash-ref mode-registry target-mode (λ () #f)))
  (unless m (error 'set-buffer-mode! "unknown mode: ~a" target-mode))
  (define km (mode-keymap m))
  (define st (mode-syntax m))
  (when km (set-buffer-keymap! buf km))
  (when st (set-buffer-syntax! buf st))
  (set-buffer-highlight-keywords! buf (mode-highlight-kw m))
  (set-buffer-highlight-syntax?! buf (mode-highlight-syntax? m))
  (set-buffer-mode-name! buf (mode-name m))
  (unless (null? (mode-highlight-kw m)) (activate-highlight! buf))
  (define afn (mode-activate m)) (when afn (afn buf)))

(define file-type->mode-name (make-parameter (hash)))

(define (init-all-mode-file-types!)
  (for ([(mn fts) (in-hash mode-file-types)])
    (for ([ft (in-list fts)])
      (file-type->mode-name (hash-set (file-type->mode-name) ft mn)))))

(define (mode-for-path path)
  (define ext (path-get-extension path))
  (hash-ref (file-type->mode-name) ext
            (λ () (hash-ref (file-type->mode-name) "" (λ () #f)))))

(define (path-get-extension path)
  (define s (if (path? path) (path->string path) path))
  (define dot-idx (let loop ([i (sub1 (string-length s))])
                    (cond [(< i 0) #f] [(char=? (string-ref s i) #\.) i] [else (loop (sub1 i))])))
  (if dot-idx (substring s dot-idx) ""))
