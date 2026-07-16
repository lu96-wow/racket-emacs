#lang racket

;; main.rkt — Minimal event loop to validate the full pipeline
;;
;; Wires together: input → command → font-lock → layout → render → output

(require "platform/termios.rkt"
         "platform/event.rkt"
         "platform/ansi.rkt"
         "kernel/data/gap.rkt"
         "kernel/data/text.rkt"
         "kernel/data/textprop.rkt"
         "kernel/buffer.rkt"
         "kernel/dirty.rkt"
         "kernel/edit.rkt"
         "kernel/key-event.rkt"
         "lang/syntax.rkt"
         "lang/font-lock.rkt"
         "display/vbuffer.rkt"
         "display/layout.rkt"
         "display/render.rkt"
         "display/face.rkt"
         "draw/terminal.rkt")

;; ============================================================
;; Key → command dispatch (naive, no keymap yet)
;; ============================================================

(define (lookup-command evt)
  (cond
    ;; Quit
    [(and (key-event? evt)
          (key-event-ctrl? evt)
          (key-event-char evt)
          (char=? (char-downcase (key-event-char evt)) #\q))
     'quit]

    ;; Backspace
    [(backspace-key? evt) cmd-backward-delete]

    ;; Return
    [(return-key? evt) cmd-newline]

    ;; C-f / C-b / C-n / C-p / C-a / C-e
    [(and (key-event? evt)
          (key-event-ctrl? evt)
          (key-event-char evt))
     (case (char-downcase (key-event-char evt))
       [(#\f) cmd-forward-char]
       [(#\b) cmd-backward-char]
       [(#\a) cmd-beginning-of-line]
       [(#\e) cmd-end-of-line]
       [else #f])]

    ;; Self-insert
    [(self-insert-key? evt)
     (λ (db evt2) (cmd-self-insert db evt2))]

    [else #f]))

;; ============================================================
;; Event loop
;; ============================================================

(define (event-loop db face-cache lang-cfg cached-vb)
  ;; ---- safe point: dirty? → fontify → layout → render → flush ----
  (if (dirty-dirty? db)
      (let* ([buf  (dirty-buffer-buf db)]
             [gb   (text-gap (buffer-text buf))]
             [tp   (buffer-text-props buf)]
             [ext  (dirty-extent db)]
             [_    (when ext (fontify-changed! gb tp lang-cfg ext))]
             [ly   (compute-layout gb (buffer-point buf)
                                   #:max-rows (terminal-height)
                                   #:max-cols (terminal-width))]
             [vb   (if (region-active? buf)
                       (render-layout/region! ly gb tp face-cache
                        (region-beginning buf) (region-end buf))
                       (render-layout! ly gb tp face-cache))]
             [_    (display format-cursor-hide)]
             [_    (terminal-flush-delta! vb cached-vb face-cache)]
             [cr   (layout-cursor-row ly)]
             [cc   (layout-cursor-col ly)]
             [_    (when (and cr cc)
                     (display (format-cursor-move cr cc)))]
             [_    (display format-cursor-show)])
        (flush-output)
        (event-loop (dirty-clear! db) face-cache lang-cfg vb))

      ;; ---- read input → dispatch → execute ----
      (let* ([evt    (read-key-event!)]
             [cmd    (lookup-command evt)])
        (cond
          [(eq? cmd 'quit) (void)]
          [(procedure? cmd)
           (define new-db (cmd db evt))
           (define new-db2 (dirty-commit! new-db))
           (event-loop new-db2 face-cache lang-cfg cached-vb)]
          [else
           (event-loop db face-cache lang-cfg cached-vb)]))))

;; ============================================================
;; Main
;; ============================================================

(define (main)
  (with-handlers ([exn:fail? (λ (e) (cleanup!) (raise e))])
    ;; Init terminal
    (screen-init!)
    (detect-color-depth!)
    (format-alt-screen-enable)
    (display format-clear-screen)
    (display format-mouse-enable)
    (flush-output)

    ;; Init face cache + language config
    (init-face-cache!)
    (define fc (current-face-cache))

    ;; Define minimal faces (render needs these symbols)
    (define-face! 'font-lock-comment-face
      (make-face-attrs attr-foreground '(100 160 100) attr-slant 'italic))
    (define-face! 'font-lock-string-face
      (make-face-attrs attr-foreground '(80 180 80)))
    (define-face! 'font-lock-keyword-face
      (make-face-attrs attr-foreground '(50 150 255) attr-weight 'bold))

    (define lang-cfg
      (make-font-lock-config
       #:syntax-table (make-racket-syntax-table)
       #:keywords (list
                   (cons #px"\\b(define|lambda|if|cond|let|let\\*|letrec|and|or|not|begin|set!|quote|quasiquote|unquote|unquote-splicing|when|unless|case|else|=>|do|delay|force|parameterize|cons|car|cdr|list|append|map|foldl|foldr|filter|apply|values|call-with-values|void)\\b"
                         'font-lock-keyword-face))))

    ;; Create scratch buffer
    (define buf (make-buffer "*scratch*"
                             (string-append
                              ";; Welcome — racket-emacs-rebuild\n"
                              ";;\n"
                              ";; (define (hello)\n"
                              ";;   (displayln \"你好, world!\"))\n"
                              ";;\n"
                              ";; Keys:  type to insert, Backspace to delete\n"
                              ";;        C-f C-b  move,  C-a C-e  line edges\n"
                              ";;        C-q quit\n"
                              "\n")))
    ;; Fontify the initial content
    (define gb (text-gap (buffer-text buf)))
    (define tp (buffer-text-props buf))
    (fontify-region! gb tp lang-cfg 0 (buffer-length buf))
    (set-buffer-point! buf 0)

    (define db (make-dirty-buffer buf))

    ;; First render
    (define ly (compute-layout gb (buffer-point buf)
                               #:max-rows (terminal-height)
                               #:max-cols (terminal-width)))
    (define vb (render-layout! ly gb tp fc))
    (display format-cursor-hide)
    (terminal-flush! vb fc)
    (display (format-cursor-move (layout-cursor-row ly)
                                  (layout-cursor-col ly)))
    (display format-cursor-show)
    (flush-output)

    ;; Enter event loop
    (with-handlers ([exn:break? (λ (e) (cleanup!) (raise e))])
      (event-loop db fc lang-cfg vb))

    (cleanup!)))

(define (cleanup!)
  (format-alt-screen-disable)
  (screen-cleanup!)
  (display "\n"))

(module+ main
  (main))
