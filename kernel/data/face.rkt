#lang racket

;; kernel/data/face.rkt — Face-Id operations on a gap-buffer
;;
;; ============================================================================
;; Pure wrappers around gap-buffer's internal faces array.
;; Does NOT know about text, markers, buffer, or undo.
;; Only knows: gap-buffer-faces + physical/logical index mapping.
;;
;; ============================================================================
;; Computation vs Application
;; ============================================================================
;;
;;   ── Pure Queries ──
;;     face-ref        gap-buffer? byte-pos → face-id (0–255)
;;     face-slice      gap-buffer? from to → bytes? (face-ids for range)
;;
;;   ── Mutations (write to gap-buffer-faces) ──
;;     face-set!       gap-buffer? byte-pos face-id → void
;;     face-fill!      gap-buffer? from to face-id → void
;;     face-copy!      gap-buffer? byte-pos face-bs → void
;;
;; ============================================================================
;; Face-Id Convention
;; ============================================================================
;;
;;   0       = default face (no highlighting)
;;   1..255  = registered faces (font-lock colors, bracket depths, region, etc.)
;;
;;   Newly inserted text always gets face-id 0.  Colorers write face-ids
;;   afterwards via face-fill! or face-set!.
;;
;; ============================================================================

(require "gap.rkt")

(provide
 face-ref face-slice
 face-set! face-fill! face-copy!)

;; ============================================================
;; Pure Queries
;; ============================================================

(define (face-ref gb byte-pos)
  ;; Face-id at logical byte-pos.  O(1).
  (unless (gap-valid-pos? gb byte-pos)
    (raise-argument-error 'face-ref
                          (format "valid position in [0, ~a]" (gap-length gb))
                          byte-pos))
  (bytes-ref (gap-buffer-faces gb) (physical-index gb byte-pos)))

(define (face-slice gb from to)
  ;; Face-ids for logical range [from, to).  Returns bytes?.
  ;; Handles gap crossing.
  (unless (gap-valid-range? gb from to)
    (raise-argument-error 'face-slice
                          (format "valid range [0, ~a]" (gap-length gb))
                          (list from to)))
  (define gs (gap-buffer-gap-start gb))
  (define ge (gap-buffer-gap-end gb))
  (define fs (gap-buffer-faces gb))
  (cond
    [(<= to gs)   (subbytes fs from to)]
    [(>= from gs) (subbytes fs (+ from (- ge gs)) (+ to (- ge gs)))]
    [else (bytes-append
           (subbytes fs from gs)
           (subbytes fs ge (+ to (- ge gs))))]))

;; ============================================================
;; Mutations
;; ============================================================

(define (face-set! gb byte-pos face-id)
  ;; Set face-id at a single logical byte position.
  (unless (gap-valid-pos? gb byte-pos)
    (raise-argument-error 'face-set!
                          (format "valid position in [0, ~a]" (gap-length gb))
                          byte-pos))
  (unless (and (exact-integer? face-id) (>= face-id 0) (<= face-id 255))
    (raise-argument-error 'face-set! "u8 face-id (0–255)" face-id))
  (bytes-set! (gap-buffer-faces gb) (physical-index gb byte-pos) face-id))

(define (face-fill! gb from to face-id)
  ;; Set face-id over logical range [from, to).
  (unless (gap-valid-range? gb from to)
    (raise-argument-error 'face-fill!
                          (format "valid range [0, ~a]" (gap-length gb))
                          (list from to)))
  (unless (and (exact-integer? face-id) (>= face-id 0) (<= face-id 255))
    (raise-argument-error 'face-fill! "u8 face-id (0–255)" face-id))

  (define gs (gap-buffer-gap-start gb))
  (define ge (gap-buffer-gap-end gb))
  (define fs (gap-buffer-faces gb))

  ;; Fill before gap
  (define before-end (min to gs))
  (for ([i (in-range from before-end)])
    (bytes-set! fs i face-id))
  ;; Fill after gap
  (define after-start (max from gs))
  (for ([i (in-range (+ after-start (- ge gs))
                     (+ to (- ge gs)))])
    (bytes-set! fs i face-id)))

(define (face-copy! gb byte-pos face-bs)
  ;; Copy face-ids from face-bs into logical range starting at byte-pos.
  ;; Each byte in face-bs sets the face-id at the corresponding position.
  (unless (and (bytes? face-bs) (gap-valid-pos? gb byte-pos))
    (raise-argument-error 'face-copy!
                          (format "bytes? and valid position in [0, ~a]" (gap-length gb))
                          (list byte-pos face-bs)))
  (define blen (bytes-length face-bs))
  (unless (<= (+ byte-pos blen) (gap-length gb))
    (raise-argument-error 'face-copy!
                          (format "enough space in [0, ~a]" (gap-length gb))
                          (list byte-pos blen)))
  (for ([i (in-range blen)])
    (face-set! gb (+ byte-pos i) (bytes-ref face-bs i))))
