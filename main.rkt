#lang racket

;; main.rkt — Event loop wiring all layers together
;;
;; Architecture (data flows top to bottom):
;;
;;   stdin (raw bytes)  ──parse──→  key-event
;;         │
;;   dispatch (cmd-self-insert, cmd-forward-char, ...)
;;         │
;;   dirty-buffer  ←─  kernel/edit.rkt commands
;;         │
;;   fontify-changed!  ←─  lang/font-lock (syntax + keywords → text-props)
;;         │
;;   calc-scroll → apply-scroll!  (point visible? → adjust leaf markers)
;;         │
;;   compute-layout  ←─  buffer gap + point + frame geometry
;;         │
;;   render-layout/cached!  ←─  layout + text-props (faces!) + row-cache  →  vbuffer
;;         │
;;   terminal-flush-delta!  ←─  vbuffer + cache  →  ANSI to stdout
;;
;; All state is explicit.  Zero globals beyond the event loop's
;; local bindings.

(require "display/vbuffer.rkt"
         "display/layout.rkt"
         "display/render.rkt"
         "display/face.rkt"
         "display/window.rkt"
         "display/char-width.rkt"
         "display/row-cache.rkt"
         "draw/terminal.rkt"
         "platform/ansi.rkt"
         "platform/termios.rkt"
         "kernel/buffer.rkt"
         "kernel/data/text.rkt"
         "kernel/data/marker.rkt"
         "kernel/data/gap.rkt"
         "kernel/dirty.rkt"
         "kernel/edit.rkt"
         "lang/apply.rkt"
         "lang/racket-lang.rkt"
         "lang/scheme-lang.rkt"
         "lang/python-lang.rkt")

;; ============================================================
;; Initial buffer content
;; ============================================================

(define initial-content
  (string-append
   ";; racket-emacs-rebuild — a pure composition of data and functions\n"
   "\n"
   ";; Architecture:\n"
   ";;   gap.rkt → text.rkt → buffer.rkt → dirty.rkt → edit.rkt\n"
   ";;   layout.rkt → render.rkt → vbuffer → terminal.rkt\n"
   ";;   window.rkt → rects → per-leaf viewport\n"
   "\n"
   ";; Keys:\n"
   ";;   arrows  — move cursor\n"
   ";;   p       — split window (same buffer)\n"
   ";;   other   — insert character\n"
   ";;   C-c     — quit\n"
   "\n"))

;; ============================================================
;; Key parsing
;; ============================================================

;; Read a single key event from stdin (raw mode, no buffering).
;; Returns one of:
;;   (list 'char ch)         — printable character
;;   (list 'escape)          — bare ESC
;;   (list 'arrow dir)       — 'up 'down 'left 'right
;;   (list 'ctrl ch)         — control character
;;   (list 'other n)         — unrecognised
(define (read-key)
  ;; Read raw bytes from stdin, parsing escape sequences.
  (define b (read-byte))
  (cond
    [(eof-object? b) '(idle)]  ; VMIN=0 VTIME=1: no data available yet
    ;; ESC — could be bare Escape or start of an escape sequence
    [(= b 27)
     (define b2 (read-byte-or-timeout))
     (cond
       [(not b2) '(escape)]
       [(= b2 91)  ;; ESC [ — CSI sequence
        (define b3 (read-byte-or-timeout))
        (case b3
          [(65) '(arrow up)]
          [(66) '(arrow down)]
          [(67) '(arrow right)]
          [(68) '(arrow left)]
          [(72) '(arrow home)]
          [(70) '(arrow end)]
          [(51) ;; ESC [ 3 ~ → Delete
           (let ([b4 (read-byte-or-timeout)])
             (if (and b4 (= b4 126)) '(delete) '(escape)))]
          [(49) ;; ESC [ 1 ~ → Home, ESC [ 1 ; 5 D → Ctrl-Left etc
           (read-and-discard-csi)]
          [(52) ;; ESC [ 4 ~ → End
           (read-and-discard-csi)]
          [(53) ;; ESC [ 5 ~ → PgUp
           (read-and-discard-csi)]
          [(54) ;; ESC [ 6 ~ → PgDn
           (read-and-discard-csi)]
          [else '(escape)])]
       [(= b2 79)  ;; ESC O — SS3 (application keypad)
        (define b3 (read-byte-or-timeout))
        (case b3
          [(72) '(arrow home)]
          [(70) '(arrow end)]
          [else '(escape)])]
       [else '(escape)])]
    ;; Backspace / DEL
    [(or (= b 127) (= b 8)) '(backspace)]
    ;; Tab
    [(= b 9) '(tab)]
    ;; Enter / Return
    [(= b 13) '(return)]
    ;; Ctrl+C → quit
    [(= b 3) '(quit)]
    ;; Ctrl+D → EOF-like
    [(= b 4) '(quit)]
    ;; Printable ASCII (space through ~)
    [(<= 32 b 126) (list 'char (integer->char b))]
    ;; Other control chars → ignore
    [(< b 32) (list 'other b)]
    ;; Non-ASCII (UTF-8 continuation or extended) — read full char
    [else
     (define bs (read-utf8-char b))
     (if bs
         (list 'char (bytes->string/utf-8 bs))
         (list 'other b))]))

;; Read a byte or return #f after a short timeout (non-blocking).
(define (read-byte-or-timeout)
  ;; In raw mode with VMIN=0 VTIME=0, read-byte returns immediately.
  ;; If no data available, returns #f.
  ;; Actually, with VMIN=0 VTIME=0, read-byte can return #<eof> for no data.
  ;; We use a small wait.  Since we set VMIN=0 VTIME=0, read-byte blocks
  ;; minimally.  For escape sequences we need to know if another byte follows.
  ;; We set VMIN=0 VTIME=1 (100ms) for this purpose.
  ;; For simplicity, just try read-byte — it returns #f/eof immediately.
  (with-handlers ([exn:fail:filesystem? (λ (e) #f)])
    (let ([b (read-byte)])
      (if (eof-object? b) #f b))))

;; Consume and discard remaining CSI sequence bytes
(define (read-and-discard-csi)
  ;; Read until we get a final byte (letter or ~) or timeout
  (let loop ()
    (define b (read-byte-or-timeout))
    (when (and b (not (member b '(65 66 67 68 72 70 126))))
      (loop)))
  '(escape))

;; Read a full UTF-8 character given the first byte
(define (read-utf8-char first-byte)
  (define len (utf8-char-len first-byte))
  (define buf (make-bytes len))
  (bytes-set! buf 0 first-byte)
  (let loop ([i 1])
    (if (>= i len)
        buf
        (let ([b (read-byte)])
          (if (eof-object? b)
              buf
              (begin (bytes-set! buf i b) (loop (add1 i))))))))

;; UTF-8 start byte length
(define (utf8-char-len b)
  (cond [(< b #x80) 1]
        [(< b #xE0) 2]
        [(< b #xF0) 3]
        [else      4]))

;; ============================================================
;; Per-leaf row cache management
;; ============================================================

(define (get-row-cache leaf-caches lf rows)
  ;; Get or create row-cache for a leaf.
  (hash-ref! leaf-caches lf (λ () (make-row-cache (max rows 100)))))

(define (invalidate-leaf-caches! leaf-caches)
  ;; Invalidate all row caches (e.g., after buffer split/resize).
  (for ([(lf rc) (in-hash leaf-caches)])
    (row-cache-invalidate! rc)))

;; ============================================================
;; Render pipeline
;; ============================================================

(define (render-frame db frm face-cache leaf-caches)
  ;; Full render pipeline:
  ;;   1. calc-scroll + apply-scroll! for each leaf (keep point visible)
  ;;   2. compute-layout → render-layout/cached! → blit to frame vbuffer
  ;; Returns (values frame-vb cursor-row cursor-col).
  ;; leaf-caches is mutated in-place.
  (define fw (frame-w frm))
  (define fh (frame-h frm))
  (define frame-vb (make-vbuffer fh fw))
  (define cursor-row 0)
  (define cursor-col 0)
  (define sel (frame-selected frm))

  (for ([lf (in-list (focus-list (frame-tree frm)))])
    (define geo (leaf-geometry frm lf))
    (when geo
      (define buf (leaf-buffer lf))
      (define gb  (text-gap (buffer-text buf)))
      (define tp  (buffer-text-props buf))

      ;; --- Scroll: keep point visible ---
      (define pt (buffer-point buf))
      (define ws (marker-pos (leaf-start lf)))
      (define rows (rect-rows geo))
      (define cols (rect-cols geo))
      (define hs  (leaf-hscroll lf))
      (define-values (new-start new-hscroll)
        (calc-scroll gb pt ws rows cols hs 'none))
      (define scroll-changed?
        (or (not (= new-start ws)) (not (= new-hscroll hs))))
      (when scroll-changed?
        (apply-scroll! lf new-start new-hscroll)
        (define rc (hash-ref leaf-caches lf #f))
        (when rc (row-cache-invalidate! rc)))

      ;; --- Layout ---
      (define ly (compute-layout gb pt
                    #:start-pos new-start
                    #:max-rows  rows
                    #:max-cols  cols
                    #:wrap-mode 'none
                    #:left-col  new-hscroll))

      ;; --- Render (with row cache) ---
      (define rc (get-row-cache leaf-caches lf rows))
      (define reg-active? (region-active? buf))
      (define leaf-vb
        (if reg-active?
            (render-layout/region/cached! ly gb tp face-cache
                                           (region-beginning buf)
                                           (region-end buf) rc)
            (render-layout/cached! ly gb tp face-cache rc)))

      ;; --- Blit into frame ---
      (vbuffer-blit! frame-vb (rect-top geo) (rect-left geo) leaf-vb)

      ;; --- Track cursor of selected leaf ---
      (when (eq? lf sel)
        (define cr (layout-cursor-row ly))
        (define cc (layout-cursor-col ly))
        (when cr (set! cursor-row (+ (rect-top geo) cr)))
        (when cc (set! cursor-col (+ (rect-left geo) cc))))))

  (values frame-vb cursor-row cursor-col))

(define (render-and-flush db frm cache face-cache leaf-caches)
  ;; Compose frame, flush delta to terminal, position cursor.
  (define-values (frame-vb cur-row cur-col)
    (render-frame db frm face-cache leaf-caches))

  (define output
    (if cache
        (terminal-flush-delta! frame-vb cache face-cache)
        (terminal-flush! frame-vb face-cache)))
  (display output)
  (display (format-cursor-move cur-row cur-col))
  (flush-output)

  ;; Return new vbuffer cache + (possibly updated) leaf-caches
  frame-vb)

;; ============================================================
;; Dispatch — key event → dirty-buffer
;; ============================================================

(define (dispatch db key frm)
  ;; Returns (values new-db new-frm action-performed?)
  (match key
    ;; Movement
    ['(arrow left)   (values (cmd-backward-char db) frm #t)]
    ['(arrow right)  (values (cmd-forward-char db) frm #t)]
    ['(arrow up)     (values (cmd-prev-line db) frm #t)]
    ['(arrow down)   (values (cmd-next-line db) frm #t)]
    ['(arrow home)   (values (cmd-beginning-of-line db) frm #t)]
    ['(arrow end)    (values (cmd-end-of-line db) frm #t)]

    ;; Deletion
    ['(backspace)    (values (cmd-backward-delete db) frm #t)]
    ['(delete)       (values (cmd-forward-delete db) frm #t)]

    ;; Newline / Tab
    ['(return)       (values (cmd-newline db) frm #t)]
    ['(tab)          (values (cmd-tab db) frm #t)]

    ;; Window split — 'p' key
    [(list 'char #\p)
     (let ([new (frame-split-leaf! frm 'vertical)])
       (values db frm #t))]

    ;; Self-insert for other printable characters
    [(list 'char ch)
     (values (cmd-self-insert db ch) frm #t)]

    ;; Escape → ignore
    ['(escape)       (values db frm #f)]

    ;; Unknown → ignore
    [_                (values db frm #f)]))

;; ============================================================
;; Main event loop
;; ============================================================

;; ============================================================
;; Language setup — populate the registry once
;; ============================================================

(set-box! available-languages
  (list racket-lang-def scheme-lang-def python-lang-def))

;; ============================================================
;; Main event loop
;; ============================================================

(define (run)
  (screen-init!)
  (detect-terminal-size!)
  (format-alt-screen-enable)
  (display format-bracketed-paste-enable)
  (display format-clear-screen)
  (flush-output)

  (dynamic-wind
    void
    (λ ()
      ;; --- Setup state ---
      (define buf (make-buffer "*scratch*" initial-content))
      (define db  (make-dirty-buffer buf))
      (init-face-cache!)
      (define fc  (current-face-cache))

      ;; Match language → activate faces → fontify entire buffer
      (fontify-setup! buf)

      (define frm (make-frame buf (terminal-width) (terminal-height)))
      (define leaf-caches (make-hasheq))
      (set-buffer-point! buf (buffer-length buf))

      ;; Initial render
      (define cache
        (with-handlers ([exn:fail? (λ (e)
                        (eprintf "Render error: ~a\n" (exn-message e)) #f)])
          (render-and-flush db frm #f fc leaf-caches)))

      ;; --- Event loop ---
      (let loop ([db db] [frm frm] [cache cache] [leaf-caches leaf-caches])
        (define key (read-key))
        (cond
          [(equal? key '(idle))  (loop db frm cache leaf-caches)]
          [(equal? key '(quit))  (void)]
          [else
           (define-values (new-db new-frm acted?)
             (dispatch db key frm))
           (define db2 (if acted? (dirty-commit! new-db) new-db))
           (when (and acted? (not (eq? frm new-frm)))
             (layout-frame! new-frm)
             (invalidate-leaf-caches! leaf-caches))
           (when (and acted? (dirty-dirty? db2))
             (define ext (dirty-extent db2))
             (when ext (fontify-change! buf ext))
             (invalidate-leaf-caches! leaf-caches))
           (define new-cache
             (if (or acted? (not (eq? frm new-frm)))
                 (with-handlers ([exn:fail? (λ (e)
                                 (eprintf "Render error: ~a\n" (exn-message e))
                                 cache)])
                   (render-and-flush db2 new-frm cache fc leaf-caches))
                 cache))
           (loop db2 new-frm new-cache leaf-caches)])))

    ;; Cleanup on exit (normal or exception)
    (λ ()
      (display format-cursor-show)
      (display format-reset)
      (display format-bracketed-paste-disable)
      (format-alt-screen-disable)
      (screen-cleanup!)
      ;; Force exit since we're in raw mode
      (exit 0))))

;; ============================================================
;; Entry point
;; ============================================================

(module+ main
  (run))
