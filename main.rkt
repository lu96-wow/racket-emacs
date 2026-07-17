#lang racket

;; main.rkt — Event loop wiring all layers together
;;
;; Data flows top to bottom on every keystroke:
;;
;;   stdin (raw bytes)
;;     │ parse
;;     ▼
;;   key-event  ──→  dispatch (cmd-self-insert, cmd-forward-char, ...)
;;     │
;;     ▼
;;   dirty-buffer  ←─  kernel/base-edit.rkt (db → db, pure)
;;     │
;;     ├─→  syntax-update!  ←─  lang/apply.rkt
;;     │      └─ syntax-scan! + keyword-scan! → writes face symbols into text-props
;;     │
;;     ├─→  calc-scroll → apply-scroll!  (point visible? → adjust leaf markers)
;;     │
;;     ├─→  compute-layout  (gap-buffer + point + geometry → visual-lines)
;;     │
;;     ├─→  render-layout/cached!  (visual-lines + text-props + row-cache → vbuffer)
;;     │
;;     └─→  terminal-flush-delta!  (vbuffer + prev-frame → ANSI → stdout)
;;
;; Module map:
;;
;;   kernel/     — data types + mutations (gap, text, buffer, dirty, edit)
;;   display/    — layout, render, vbuffer, window tree, faces, row-cache
;;   draw/       — vbuffer → ANSI terminal output
;;   platform/   — raw terminal I/O, ANSI escape sequences
;;   lang/       — syntax highlighting
;;     syntax.rkt      — syntax-table data type (character classes, multi-char rules)
;;     font-lock.rkt   — scanning engine (syntax-scan!, keyword-scan!, syntax-config)
;;     define.rkt      — lang-def data type (faces + keywords + syntax-table)
;;     apply.rkt       — unified entry (syntax-setup!, syntax-update!)
;;     *-lang.rkt      — language data: racket, scheme, python
;;   input/      — keyboard input
;;     key.rkt         — key-char | key-ctrl | key-sym (three disjoint types)
;;     parse.rkt       — raw bytes → key event
;;     keymap.rkt      — keymap (hash key→command) + dispatch
;;
;; All state is explicit.  Zero globals beyond the event loop's
;; local bindings and the face-cache / language registry.

(require "display/vbuffer.rkt"
         "display/layout.rkt"
         "display/render.rkt"
         "display/face.rkt"
         "display/window.rkt"
         "kernel/data/char-width.rkt"
         "display/row-cache.rkt"
         "draw/terminal.rkt"
         "platform/ansi.rkt"
         "platform/termios.rkt"
         "kernel/buffer.rkt"
         "kernel/data/text.rkt"
         "kernel/data/marker.rkt"
         "kernel/data/gap.rkt"
         "kernel/data/query.rkt"
         "kernel/dirty.rkt"
         "kernel/base-edit.rkt"
         "lang/apply.rkt"
         "lang/racket-lang.rkt"
         "lang/scheme-lang.rkt"
         "lang/python-lang.rkt"
         "input/key.rkt"
         "input/parse.rkt"
         "input/keymap.rkt")

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
   ";;   C-v       — split window (same buffer)\n"
   ";;   other   — insert character\n"
   ";;   C-c     — quit\n"
   "\n"))

;; ============================================================
;; editor-buffer — extends kernel buffer with editor-layer data
;;
;; Wraps a kernel buffer with syntax config and optional local keymap.
;; kernel/ never sees this — it only knows about plain buffer?.
;; Data stays with the buffer instead of in external lookup tables.
;; ============================================================

(struct editor-buffer (buf syntax-config local-keymap) #:transparent)

;; ============================================================
;; Mouse handlers — terminal (x,y) → buffer position pipeline
;; ============================================================
;; Architecture (inverse of render-frame's forward pipeline):
;;   Terminal mouse (x,y) [1-based SGR]
;;     → frame-xy->leaf frm x y          [1→0 based, leaf resolution]
;;     → leaf-geometry frm lf             [rect boundary check]
;;     → leaf-xy->buffer-pos lf geo x y   [local row,col + layout → byte-pos]
;;     → dirty-set-point! / scroll        [apply to buffer state]
;;
;; Window boundaries are inherently correct because:
;;   1. leaf-at-xy uses half-open intervals that partition the screen exactly
;;   2. leaf-xy->buffer-pos recomputes the same layout as render-frame
;;   3. 0-based conversion across all internal systems; 1→0 SGR happens once

(define (handle-mouse-set-point! db frm ke)
  ;; Move point to the buffer position under the mouse cursor.
  ;; window.rkt handles all coordinate math and focus switching.
  (values db frm
          (frame-point-to-xy! frm (key-mouse-x ke) (key-mouse-y ke))))

(define (handle-mouse-scroll! db frm ke)
  ;; Scroll the leaf under the mouse cursor.
  ;; Returns (values db frm acted?).
  (define lf (frame-xy->leaf frm (key-mouse-x ke) (key-mouse-y ke)))
  (if lf
      (let* ()
        (define buf (leaf-buffer lf))
        (define gb  (text-gap (buffer-text buf)))
        (define ws  (marker-pos (leaf-start lf)))
        (define dir (key-mouse-button ke))
        (define lines 3)
        (define (newline-byte? b) (= b #x0A))
        (define new-start
          (if (eq? dir 'wheel-up)
              (let loop ([p ws] [n lines])
                (if (or (<= p 0) (zero? n))
                    p
                    (let ([nl (gap-scan-byte gb (max 0 (sub1 p)) 'backward newline-byte?)])
                      (if (< nl 0) 0 (loop nl (sub1 n))))))
              (let loop ([p ws] [n lines])
                (if (or (>= p (gap-length gb)) (zero? n))
                    p
                    (let ([nl (gap-scan-byte gb p 'forward newline-byte?)])
                      (if (>= nl (gap-length gb))
                          (gap-length gb)
                          (loop (add1 nl) (sub1 n))))))))
        (when (not (= new-start ws))
          (apply-scroll! lf new-start (leaf-hscroll lf)))
        (values db frm #t))
      (values db frm #t)))

;; ============================================================
;; ============================================================
;; Global keymap — pure data, constructed once (analogous to lang-def)
;; ============================================================

(define global-keymap
  (make-keymap
   ;; Arrow / nav keys
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

   ;; Control keys
   (cons (key-ctrl #\a) (edit-cmd cmd-beginning-of-line))
   (cons (key-ctrl #\e) (edit-cmd cmd-end-of-line))
   (cons (key-ctrl #\f) (edit-cmd cmd-forward-char))
   (cons (key-ctrl #\b) (edit-cmd cmd-backward-char))
   (cons (key-ctrl #\p) (edit-cmd cmd-prev-line))
   (cons (key-ctrl #\n) (edit-cmd cmd-next-line))
   (cons (key-ctrl #\d) (edit-cmd cmd-forward-delete))
   (cons (key-ctrl #\k) (edit-cmd cmd-kill-line))
   (cons (key-ctrl #\y) (edit-cmd cmd-yank))
   (cons (key-ctrl #\_) (edit-cmd cmd-undo))
   (cons (key-ctrl #\r) (edit-cmd cmd-redo))

   ;; Window
   (cons (key-ctrl #\v)
         (window-cmd (λ (frm) (frame-split-leaf! frm 'vertical) frm)))
   (cons (key-ctrl #\o)
         (window-cmd (λ (frm) (frame-select-next! frm) frm)))

   ;; Resize — dispatched like any other key
   (cons (key-sym 'resize)
         (window-cmd (λ (frm)
                       (detect-terminal-size!)
                       (frame-resize frm (terminal-width) (terminal-height)))))

   ;; ── Mouse: click to set point ──
   ;; Left click at any terminal position → move point there.
   ;; Works across all leaves; clicks on non-selected leaves switch focus.
   ;; Uses the unified frame-xy->leaf + leaf-xy->buffer-pos pipeline.
   (cons (cons 'left 'press)
         (mouse-cmd handle-mouse-set-point!))
   (cons (cons 'left 'move)
         (mouse-cmd handle-mouse-set-point!))
   ;; Scroll wheel
   (cons (cons 'wheel-up 'scroll)
         (mouse-cmd handle-mouse-scroll!))
   (cons (cons 'wheel-down 'scroll)
         (mouse-cmd handle-mouse-scroll!))))

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
      (define pt (marker-pos (leaf-point lf)))
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
;; Main event loop
;; ============================================================

(define (run)
  (screen-init!)
  (detect-terminal-size!)
  (format-alt-screen-enable)
  (display format-bracketed-paste-enable)
  (display format-mouse-enable)
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
      (define languages (list racket-lang-def scheme-lang-def python-lang-def))

      ;; Match language → activate faces → scan → returns syntax-config
      ;; No external table: cfg lives in editor-buffer struct.
      (define cfg (syntax-setup! buf languages))
      (define ebuf (editor-buffer buf cfg #f))

      (define frm (make-frame buf (terminal-width) (terminal-height)))
      (define leaf-caches (make-hasheq))
      (set-buffer-point! buf (buffer-length buf))

      ;; Initial render
      (define cache
        (with-handlers ([exn:fail? (λ (e)
                        (eprintf "Render error: ~a\n" (exn-message e)) #f)])
          (render-and-flush db frm #f fc leaf-caches)))

      ;; --- Event loop ---
      (let loop ([db db] [frm frm] [cache cache] [leaf-caches leaf-caches] [ebuf ebuf])
        (define ke (read-key))
        (cond
          [(key-idle? ke)  (loop db frm cache leaf-caches ebuf)]
          [(key-quit? ke)  (void)]
          [else
           ;; Resolve keymap: local (from ebuf) or fallback to global
           (define km (keymap-resolve (editor-buffer-local-keymap ebuf) global-keymap))
           (define-values (new-db new-frm acted?)
             (dispatch-key km db frm ke cmd-self-insert))
           (define db2 (if acted? (dirty-commit! new-db) new-db))
           (when (and acted? (not (eq? frm new-frm)))
             (layout-frame! new-frm)
             (invalidate-leaf-caches! leaf-caches))
           (when (and acted? (dirty-dirty? db2))
             (define ext (dirty-extent db2))
             (when ext (syntax-update! (editor-buffer-syntax-config ebuf) (editor-buffer-buf ebuf) ext))
             (invalidate-leaf-caches! leaf-caches))
           (define new-cache
             (if (or acted? (not (eq? frm new-frm)))
                 (with-handlers ([exn:fail? (λ (e)
                                 (eprintf "Render error: ~a\n" (exn-message e))
                                 cache)])
                   (render-and-flush db2 new-frm cache fc leaf-caches))
                 cache))
           (loop db2 new-frm new-cache leaf-caches ebuf)])))

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
