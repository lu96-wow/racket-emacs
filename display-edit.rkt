#lang racket

;; display-edit.rkt — Viewport-aware display pipeline
;;
;; Provides the unified post-edit pipeline.
;; Does NOT import edit.rkt — avoids prefix-in module hang.
;; main.rkt uses edit.rkt for commands and display-edit for display.

(require "display/layout.rkt"
         "display/render.rkt"
         "display/vbuffer.rkt"
         "display/window.rkt"
         "display/face.rkt"
         "display/row-cache.rkt"
         "draw/terminal.rkt"
         "platform/ansi.rkt"
         "kernel/dirty.rkt"
         "kernel/buffer.rkt"
         "kernel/data/text.rkt"
         "kernel/data/marker.rkt")

(provide redisplay! redisplay-init! invalidate-leaf-caches!)

;; ── Viewport Sync ──
(define (sync-viewport! frm leaf-caches)
  (for/or ([lf (in-list (focus-list (frame-tree frm)))])
    (define geo (leaf-geometry frm lf))
    (and geo
         (let* ([buf  (leaf-buffer lf)]
                [gb   (text-gap (buffer-text buf))]
                [pt   (marker-pos (leaf-point lf))]
                [ws   (marker-pos (leaf-start lf))]
                [rows (rect-rows geo)]
                [cols (rect-cols geo)]
                [hs   (leaf-hscroll lf)])
           (define-values (new-start new-hscroll)
             (calc-scroll gb pt ws rows cols hs 'none))
           (define changed? (or (not (= new-start ws)) (not (= new-hscroll hs))))
           (when changed?
             (apply-scroll! lf new-start new-hscroll)
             (define rc (hash-ref leaf-caches lf #f))
             (when rc (row-cache-invalidate! rc)))
           changed?))))

;; ── Render ──
(define (render-frame frm reg leaf-caches)
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
      (define ly (compute-layout gb pt
                    #:start-pos ws #:max-rows rows #:max-cols cols
                    #:wrap-mode 'none #:left-col hs))
      (define rc (hash-ref! leaf-caches lf (λ () (make-row-cache (max rows 100)))))
      (define reg-active? (region-active? buf))
      (define leaf-vb
        (if reg-active?
            (render-layout/region/cached! ly gb reg
               (region-beginning buf) (region-end buf) rc)
            (render-layout/cached! ly gb reg rc)))
      (vbuffer-blit! frame-vb (rect-top geo) (rect-left geo) leaf-vb)
      (when (eq? lf sel)
        (define cr (layout-cursor-row ly))
        (define cc (layout-cursor-col ly))
        (when cr (set! cursor-row (+ (rect-top geo) cr)))
        (when cc (set! cursor-col (+ (rect-left geo) cc))))))
  (values frame-vb cursor-row cursor-col))

(define (flush-frame vb cache-vb fc cur-row cur-col)
  (define output
    (if cache-vb
        (terminal-flush-delta! vb cache-vb fc)
        (terminal-flush! vb fc)))
  (display output)
  (display (format-cursor-move cur-row cur-col))
  (flush-output))

(define (render-and-flush! frm reg leaf-caches cache-vb)
  (define-values (new-vb cr cc)
    (with-handlers ([exn:fail? (λ (e)
                    (eprintf "Render error: ~a\n" (exn-message e))
                    (values cache-vb 0 0))])
      (render-frame frm reg leaf-caches)))
  (flush-frame new-vb cache-vb (face-registry-cache reg) cr cc)
  new-vb)

;; ── Pipeline ──
;;
;;   dirty-clear!  →  layout  →  sync-scroll  →  render+flush
;;
;; Each stage is a named function with explicit inputs/outputs
;; for testability and inspection.
;;
;;   dirty-commit! is NOT here — main.rkt does that before calling
;;   redisplay! because lang-layer colorers (font-lock) need the
;;   committed state.

;; ── Stage 1: dirty-clear! ──

(define (dirty-clear-stage db content?)
  ;; Clear the change marker.  Returns fresh db.
  ;; Caller must have already called dirty-commit! before colorers.
  (if (and content? (dirty-dirty? db))
      (dirty-clear! db)
      db))

;; ── Stage 2: layout ──

(define (layout-stage frm leaf-caches frame?)
  ;; Recalculate window geometry.  Mutates frm + invalidates caches.
  ;; Returns void — side effects only.
  (when frame?
    (layout-frame! frm)
    (invalidate-leaf-caches! leaf-caches)))

;; ── Stage 3: sync scroll ──

(define (sync-stage frm leaf-caches)
  ;; Adjust scroll to keep point visible in each leaf.
  ;; Returns boolean? — #t if any leaf's scroll changed.
  (sync-viewport! frm leaf-caches))

;; ── Stage 4: render + flush ──

(define (render-stage frm reg leaf-caches cache-vb should-render?)
  ;; Build frame vbuffer, diff against cache, output to terminal.
  ;; Returns the new cache vbuffer (or cache-vb unchanged if no render).
  (if should-render?
      (render-and-flush! frm reg leaf-caches cache-vb)
      cache-vb))

;; ── redisplay! — composed pipeline ──

(define (redisplay! db frm reg leaf-caches cache-vb
                    #:content-changed? [content? #f]
                    #:frame-changed?  [frame?  #f])
  (define db2      (dirty-clear-stage db content?))
  (layout-stage frm leaf-caches frame?)
  (define scrolled? (sync-stage frm leaf-caches))
  (define new-vb   (render-stage frm reg leaf-caches cache-vb
                                 (or content? frame? scrolled?)))
  (values db2 frm new-vb leaf-caches))

(define (redisplay-init! frm reg leaf-caches)
  (sync-viewport! frm leaf-caches)
  (render-and-flush! frm reg leaf-caches #f))

(define (invalidate-leaf-caches! leaf-caches)
  (for ([(lf rc) (in-hash leaf-caches)]) (row-cache-invalidate! rc)))
