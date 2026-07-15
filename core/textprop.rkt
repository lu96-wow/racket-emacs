#lang racket

;; base/textprop.rkt — Text Properties via interval-map
;;
;; Uses Racket's data/interval-map (skip-list) for efficient
;; interval storage and automatic gap adjustment on insert/delete.
;; Dependency: buffer.rkt, gap.rkt

(require data/interval-map
         "../kernel/buffer.rkt")

(provide
 put-text-property
 get-text-property
 remove-text-properties
 buffer-prop-map
 init-buffer-text-properties!
 face-at-pos
 ;; paren-depth (separate interval-map, co-exists with face)
 put-paren-depth
 get-paren-depth
 clear-paren-depth!
 paren-depth-map
 paren-depth-adjust!)

;; ============================================================
;; Per-buffer storage
;; ============================================================

(define buffer-prop-table (make-hasheq))  ; buffer → interval-map

;; Cleanup hook registered in base/ (kernel has no buffer registry)

(define (buffer-prop-map buf)
  (hash-ref buffer-prop-table buf
    (λ ()
      (define im (make-interval-map))
      (hash-set! buffer-prop-table buf im)
      im)))

;; ============================================================
;; init-buffer-text-properties!
;; ============================================================

(define (init-buffer-text-properties! buf)
  (when (not (buffer-var buf 'textprop-initialized? #f))
    (set-buffer-var! buf 'textprop-initialized? #t)
    (buffer-prop-map buf)   ; ensure face interval-map exists
    (paren-depth-map buf)   ; ensure paren-depth interval-map exists
    ;; Register after-change hook for face interval-map adjustment.
    ;; (paren-depth adjustment is registered separately in
    ;;  font-lock-activate so it always runs before fontification.)
    (define hm (buffer-hooks buf))
    (set-hook-manager-after-fns! hm
      (append (hook-manager-after-fns hm)
              (list (λ (b start lendel lenins)
                      (define im (buffer-prop-map b))
                      (cond [(positive? lenins)
                             (interval-map-expand! im start (+ start lenins))]
                            [(positive? lendel)
                             (interval-map-contract! im start (+ start lendel))]
                            [else (void)])))))))

;; ============================================================
;; put-text-property — set key→value on [start, end)
;; ============================================================

(define (put-text-property buf start end key value)
  ;; interval-map-set! automatically splits/merges overlapping intervals.
  ;; For font-lock: the keyword pass checks get-text-property before calling,
  ;; so we only write new faces on previously-unfaced ranges.
  (interval-map-set! (buffer-prop-map buf) start end (hasheq key value)))

;; ============================================================
;; get-text-property
;; ============================================================

(define (get-text-property buf pos key [default #f])
  (define im (buffer-prop-map buf))
  (define props (interval-map-ref im pos (λ () (hasheq))))
  (hash-ref props key default))

;; ============================================================
;; remove-text-properties
;; ============================================================

(define (remove-text-properties buf start end keys)
  ;; Guard against empty range (interval-map-remove! requires start < end)
  (when (< start end)
    (interval-map-remove! (buffer-prop-map buf) start end)))

;; ============================================================
;; face-at-pos — convenience for render
;; ============================================================

(define (face-at-pos buf pos)
  (get-text-property buf pos 'face #f))

;; ============================================================
;; paren-depth — separate interval-map for rainbow parens
;; ============================================================

(define paren-depth-table (make-hasheq))  ; buffer → interval-map

(define (paren-depth-map buf)
  (hash-ref paren-depth-table buf
    (λ ()
      (define im (make-interval-map))
      (hash-set! paren-depth-table buf im)
      im)))

(define (put-paren-depth buf start end depth)
  (when (< start end)
    (interval-map-set! (paren-depth-map buf) start end depth)))

(define (get-paren-depth buf pos [default #f])
  (define im (hash-ref paren-depth-table buf (λ () #f)))
  (if im (interval-map-ref im pos (λ () default)) default))

(define (clear-paren-depth! buf start end)
  (when (< start end)
    (define im (hash-ref paren-depth-table buf (λ () #f)))
    (when im (interval-map-remove! im start end))))

;; ── after-change hook helper: keep paren-depth map in sync ──
(define (paren-depth-adjust! b start lendel lenins)
  (define pd (paren-depth-map b))
  (cond [(positive? lenins)
         (interval-map-expand! pd start (+ start lenins))]
        [(positive? lendel)
         (interval-map-contract! pd start (+ start lendel))]
        [else (void)]))
