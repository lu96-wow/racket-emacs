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
         "kernel/data/marker.rkt"
         "kernel/bracket-colorer.rkt"
         "kernel/font-lock.rkt")

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
(define (render-frame frm face-cache leaf-caches)
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
            (render-layout/region/cached! ly gb face-cache
               (region-beginning buf) (region-end buf) rc)
            (render-layout/cached! ly gb face-cache rc)))
      (vbuffer-blit! frame-vb (rect-top geo) (rect-left geo) leaf-vb)
      (when (eq? lf sel)
        (define cr (layout-cursor-row ly))
        (define cc (layout-cursor-col ly))
        (when cr (set! cursor-row (+ (rect-top geo) cr)))
        (when cc (set! cursor-col (+ (rect-left geo) cc))))))
  (values frame-vb cursor-row cursor-col))

(define (flush-frame vb cache-vb face-cache cur-row cur-col)
  (define output
    (if cache-vb
        (terminal-flush-delta! vb cache-vb face-cache)
        (terminal-flush! vb face-cache)))
  (display output)
  (display (format-cursor-move cur-row cur-col))
  (flush-output))

(define (render-and-flush! frm fc leaf-caches cache-vb)
  (define-values (new-vb cr cc)
    (with-handlers ([exn:fail? (λ (e)
                    (eprintf "Render error: ~a\n" (exn-message e))
                    (values cache-vb 0 0))])
      (render-frame frm fc leaf-caches)))
  (flush-frame new-vb cache-vb fc cr cc)
  new-vb)

;; ── Pipeline ──
(define (redisplay! db frm fc leaf-caches cache-vb
                    #:content-changed? [content? #f]
                    #:frame-changed?  [frame?  #f]
                    #:bracket-colorer [bkt #f]
                    #:syntax-table    [st   #f]
                    #:font-locker     [fl   #f])
  (define db1 (if (and content? (dirty-dirty? db)) (dirty-commit! db) db))
  ;; Bracket depth coloring — after commit (text is final), before clear
  (when (and bkt st (dirty-dirty? db1))
    (define chg (dirty-change db1))
    (when chg
      (define buf (dirty-buffer-buf db1))
      (define gb  (text-gap (buffer-text buf)))
      (bracket-colorer-update! bkt gb st (car chg) (cdr chg)))
    (invalidate-leaf-caches! leaf-caches))
  ;; Font-lock syntax highlighting — after bracket coloring, overwrites
  ;; bracket faces where font-lock has its own data (string, comment, etc.)
  (when (and fl (dirty-dirty? db1))
    (define chg (dirty-change db1))
    (when chg
      (define buf (dirty-buffer-buf db1))
      (define gb  (text-gap (buffer-text buf)))
      (font-lock-update! fl gb (car chg) (cdr chg)))
    (invalidate-leaf-caches! leaf-caches))
  (when frame? (layout-frame! frm) (invalidate-leaf-caches! leaf-caches))
  (define db2 (dirty-clear! db1))
  (define scrolled? (sync-viewport! frm leaf-caches))
  (if (or content? frame? scrolled?)
      (let ([new-vb (render-and-flush! frm fc leaf-caches cache-vb)])
        (values db2 frm new-vb leaf-caches))
      (values db2 frm cache-vb leaf-caches)))

(define (redisplay-init! frm fc leaf-caches)
  (sync-viewport! frm leaf-caches)
  (render-and-flush! frm fc leaf-caches #f))

(define (invalidate-leaf-caches! leaf-caches)
  (for ([(lf rc) (in-hash leaf-caches)]) (row-cache-invalidate! rc)))
