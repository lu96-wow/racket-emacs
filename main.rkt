#lang racket

;; main.rkt — Event loop wiring all layers together
;;
;; ============================================================================
;; Data Flow (every keystroke)
;; ============================================================================
;;
;;   stdin (raw bytes)  ──→  input/parse.rkt  ──→  key-event
;;     │
;;     ▼
;;   dispatch-key  ──→  edit.rkt commands  ──→  dirty-buffer (db)
;;     │
;;     ▼
;;   dirty-commit!  (marks undo boundary)
;;     │
;;     ├──  [future: colorer-on-edit!  — lang/incremental-colorer]
;;     ├──  [future: bracket-update!   — lang/bracket-cache]
;;     │
;;     ▼
;;   dirty-clear!  (reset change marker)
;;     │
;;     ▼
;;   render-frame  ──→  display/layout + display/render  ──→  vbuffer
;;     │
;;     ▼
;;   terminal-flush-delta!  ──→  ANSI → stdout
;;
;; ============================================================================
;; Modules
;; ============================================================================
;;
;;   kernel/     — data types + editing (gap, text, buffer, dirty, edit)
;;   display/    — layout, render, vbuffer, window tree, faces
;;   draw/       — vbuffer → ANSI terminal output
;;   platform/   — raw terminal I/O, ANSI escape sequences
;;   input/      — keyboard input parsing + keymap dispatch
;;
;; ============================================================================

(require "display/vbuffer.rkt"
         "display/layout.rkt"
         "display/render.rkt"
         "display/face.rkt"
         "display/window.rkt"
         "display/row-cache.rkt"
         "kernel/data/char-width.rkt"
         "draw/terminal.rkt"
         "platform/ansi.rkt"
         "platform/termios.rkt"
         "kernel/buffer.rkt"
         "kernel/data/text.rkt"
         "kernel/data/marker.rkt"
         "kernel/data/face.rkt"
         "kernel/dirty.rkt"
         "edit.rkt"
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
   ";;   C-f C-b C-n C-p     move cursor\n"
   ";;   C-a C-e             beginning/end of line\n"
   ";;   C-u                 forward-delete\n"
   ";;   C-k                 kill line\n"
   ";;   C-y                 yank (paste)\n"
   ";;   C-z C-x             undo / redo\n"
   ";;   C-c                 quit\n"
   "\n"))

;; ============================================================
;; Command wrappers — uniform (db frm → db frm) or (db frm ke → db frm)
;; ============================================================

(define (edit db frm fn)
  (values (fn db) frm #t))

(define (move db frm fn)
  (values (fn db) frm #f))

(define (window db frm fn)
  (values db (fn frm) #t))

;; ============================================================
;; Global keymap — pure data
;; ============================================================

(define global-keymap
  (make-keymap
   ;; ── Arrow / nav keys ──
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

   ;; ── Control keys ──
   (cons (key-ctrl #\a) (edit-cmd cmd-beginning-of-line))
   (cons (key-ctrl #\e) (edit-cmd cmd-end-of-line))
   (cons (key-ctrl #\f) (edit-cmd cmd-forward-char))
   (cons (key-ctrl #\b) (edit-cmd cmd-backward-char))
   (cons (key-ctrl #\p) (edit-cmd cmd-prev-line))
   (cons (key-ctrl #\n) (edit-cmd cmd-next-line))
   (cons (key-ctrl #\u) (edit-cmd cmd-forward-delete))
   (cons (key-ctrl #\k) (edit-cmd cmd-kill-line))
   (cons (key-ctrl #\y) (edit-cmd cmd-yank))
   (cons (key-ctrl #\z) (edit-cmd cmd-undo))
   (cons (key-ctrl #\x) (edit-cmd cmd-redo))

   ;; ── Window ──
   (cons (key-ctrl #\v)
         (window-cmd (λ (frm) (frame-split-leaf! frm 'vertical) frm)))
   (cons (key-ctrl #\o)
         (window-cmd (λ (frm) (frame-select-next! frm) frm)))

   ;; ── Resize ──
   (cons (key-sym 'resize)
         (window-cmd (λ (frm)
                       (detect-terminal-size!)
                       (frame-resize frm (terminal-width) (terminal-height)))))

   ;; ── Mouse ──
   (cons (cons 'left 'press)
         (mouse-cmd (λ (db frm ke)
                      (define acted? (frame-point-to-xy! frm (key-mouse-x ke) (key-mouse-y ke)))
                      (values db frm acted?))))))

;; ============================================================
;; Render pipeline
;; ============================================================

(define (render-frame db frm face-cache leaf-caches)
  ;; Compute scroll + layout + render for all leaves.
  ;; Returns (values frame-vb cursor-row cursor-col).
  ;;
  ;; KEY DESIGN: scroll calculation is PURE (calc-scroll).
  ;; apply-scroll! is called OUTSIDE the render to mutate leaf markers.
  ;; The render itself reads leaf state and produces a vbuffer.
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
      (define pt  (marker-pos (leaf-point lf)))
      (define ws  (marker-pos (leaf-start lf)))
      (define rows (rect-rows geo))
      (define cols (rect-cols geo))
      (define hs  (leaf-hscroll lf))

      ;; ── Step 1: Pure scroll calculation ──
      (define-values (new-start new-hscroll)
        (calc-scroll gb pt ws rows cols hs 'none))

      ;; ── Step 2: Apply scroll (mutation) ──
      (define scroll-changed?
        (or (not (= new-start ws)) (not (= new-hscroll hs))))
      (when scroll-changed?
        (apply-scroll! lf new-start new-hscroll)
        (define rc (hash-ref leaf-caches lf #f))
        (when rc (row-cache-invalidate! rc)))

      ;; ── Step 3: Pure layout ──
      (define ly (compute-layout gb pt
                    #:start-pos new-start
                    #:max-rows  rows
                    #:max-cols  cols
                    #:wrap-mode 'none
                    #:left-col  new-hscroll))

      ;; ── Step 4: Render (with row cache) ──
      (define rc (hash-ref! leaf-caches lf
                            (λ () (make-row-cache (max rows 100)))))
      (define reg-active? (region-active? buf))
      (define leaf-vb
        (if reg-active?
            (render-layout/region/cached! ly gb face-cache
                                           (region-beginning buf)
                                           (region-end buf) rc)
            (render-layout/cached! ly gb face-cache rc)))

      ;; ── Step 5: Blit leaf vbuffer into frame vbuffer ──
      (vbuffer-blit! frame-vb (rect-top geo) (rect-left geo) leaf-vb)

      ;; ── Step 6: Track cursor of selected leaf ──
      (when (eq? lf sel)
        (define cr (layout-cursor-row ly))
        (define cc (layout-cursor-col ly))
        (when cr (set! cursor-row (+ (rect-top geo) cr)))
        (when cc (set! cursor-col (+ (rect-left geo) cc))))))

  (values frame-vb cursor-row cursor-col))

(define (flush-frame vb cache-vb face-cache cur-row cur-col)
  ;; Compose terminal output string.
  (define output
    (if cache-vb
        (terminal-flush-delta! vb cache-vb face-cache)
        (terminal-flush! vb face-cache)))
  (display output)
  (display (format-cursor-move cur-row cur-col))
  (flush-output))

;; ============================================================
;; Event loop helpers
;; ============================================================

(define (get-row-cache leaf-caches lf rows)
  (hash-ref! leaf-caches lf (λ () (make-row-cache (max rows 100)))))

(define (invalidate-leaf-caches! leaf-caches)
  (for ([(lf rc) (in-hash leaf-caches)])
    (row-cache-invalidate! rc)))

;; ============================================================
;; Main event loop
;; ============================================================

(define (run)
  (dynamic-wind
    void
    (λ ()
      ;; ── Terminal setup (inside dynamic-wind so cleanup always runs) ──
      (screen-init!)
      (detect-terminal-size!)
      (format-alt-screen-enable)
      (display format-mouse-enable)
      (display format-clear-screen)
      (flush-output)

      ;; ── State setup ──
      (define buf (make-buffer "*scratch*" initial-content))
      (define db  (make-dirty-buffer buf))
      (init-face-cache!)
      (define fc (current-face-cache))

      ;; Set point to end of buffer
      (set-buffer-point! buf (buffer-length buf))

      ;; Create frame (window tree)
      (define frm (make-frame buf (terminal-width) (terminal-height)))
      (define leaf-caches (make-hasheq))

      ;; ── Initial render ──
      (define-values (vb _cr _cc)
        (with-handlers ([exn:fail? (λ (e)
                        (eprintf "Render error: ~a\n" (exn-message e))
                        (make-vbuffer (terminal-height) (terminal-width)))])
          (render-frame db frm fc leaf-caches)))
      (define cache-vb vb)
      (flush-frame vb #f fc _cr _cc)

      ;; ── Event loop ──
      (let loop ([db db] [frm frm] [cache-vb cache-vb] [leaf-caches leaf-caches])
        (define ke (read-key))

        (cond
          ;; Quit
          [(key-quit? ke) (void)]

          ;; Idle — nothing to do (no background colorer yet)
          [(key-idle? ke)
           (loop db frm cache-vb leaf-caches)]

          ;; Resize already handled via keymap/key-sym dispatch
          ;; → but read-key may return 'resize before dispatch
          [(and (key-sym? ke) (eq? (key-sym-name ke) 'resize))
           (detect-terminal-size!)
           (define new-frm (frame-resize frm (terminal-width) (terminal-height)))
           (invalidate-leaf-caches! leaf-caches)
           (define-values (new-vb cr cc)
             (render-frame db new-frm fc leaf-caches))
           (flush-frame new-vb #f fc cr cc)
           (loop db new-frm new-vb leaf-caches)]

          ;; Normal key → dispatch
          [else
           ;; Step 1: dispatch command
           (define-values (new-db new-frm acted?)
             (dispatch-key global-keymap db frm ke cmd-self-insert))

           ;; Step 2: commit undo boundary (if content changed)
           (define db2
             (if (and acted? (dirty-dirty? new-db))
                 (dirty-commit! new-db)
                 new-db))

           ;; Step 3: handle window structure changes
           (when (and acted? (not (eq? frm new-frm)))
             (layout-frame! new-frm)
             (invalidate-leaf-caches! leaf-caches))

           ;; Step 4: clear change marker (colorer would process it here)
           (define db3 (dirty-clear! db2))

           ;; Step 5: render if anything changed
           (if (or acted? (not (eq? frm new-frm)))
               (let ()
                 (define-values (new-vb cr cc)
                   (with-handlers ([exn:fail? (λ (e)
                                   (eprintf "Render error: ~a\n" (exn-message e))
                                   (values cache-vb 0 0))])
                     (render-frame db3 new-frm fc leaf-caches)))
                 (flush-frame new-vb cache-vb fc cr cc)
                 (loop db3 new-frm new-vb leaf-caches))
               (loop db3 new-frm cache-vb leaf-caches))])))

    ;; ── Cleanup on exit (normal or exception) ──
    (λ ()
      (display format-cursor-show)
      (display format-reset)
      (format-alt-screen-disable)
      (screen-cleanup!)
      (exit 0))))

;; ============================================================
;; Entry point
;; ============================================================

(module+ main
  (run))
