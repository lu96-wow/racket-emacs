#lang racket

;; kernel/data/gap.rkt — Bytes-Based Gap Buffer
;;
;; ============================================================================
;; Data Layout
;; ============================================================================
;;
;;   [text-before-gap] [gap] [text-after-gap]
;;   ↑ 0             ↑ gs    ↑ ge            ↑ bytes-length
;;
;; The gap buffer also carries a `faces` array (same length as `bytes`)
;; for O(1) face-id lookup per byte.  The faces array is automatically
;; maintained by move/resize/insert/delete, but gap.rkt does NOT expose
;; face operations — those live in kernel/data/face.rkt.
;;
;; ============================================================================
;; Invariants
;; ============================================================================
;;
;;   I1. 0 ≤ gap-start ≤ gap-end ≤ (bytes-length bytes)
;;   I2. (bytes-length bytes) = (bytes-length faces)
;;   I3. The gap always sits at the last mutation point.
;;   I4. gap area in `faces` is always zero-filled.
;;
;; ============================================================================
;; Public API (text only, no face operations)
;; ============================================================================
;;
;;   gap-length gap-byte-ref gap-subbytes           — pure text queries
;;   gap-valid-pos? gap-valid-range?                 — validation
;;   gap-insert! gap-delete!                         — text mutations
;;   gap-buffer-bytes gap-buffer-faces               — raw access for face.rkt
;;   gap-buffer-gap-start gap-buffer-gap-end        — raw access for query.rkt
;;   physical-index logical-index                    — index mapping
;;
;; ============================================================================

(provide
 make-gap-buffer gap-buffer?
 gap-length gap-byte-ref gap-subbytes
 gap-valid-pos? gap-valid-range?
 gap-insert! gap-delete!
 gap-buffer-bytes gap-buffer-faces
 gap-buffer-gap-start gap-buffer-gap-end
 physical-index logical-index)

;; ============================================================
;; Struct
;; ============================================================

(struct gap-buffer
  ([bytes #:mutable]     ; bytes? — text content (includes gap)
   [faces #:mutable]     ; bytes? — face-ids (same length as bytes, u8 per byte)
   [gap-start #:mutable] ; byte-pos — first byte of the gap
   [gap-end #:mutable])  ; byte-pos — first byte after the gap
  #:transparent)

;; ============================================================
;; Constructor
;; ============================================================

(define (make-gap-buffer [initial-text ""])
  (define init-bs (string->bytes/utf-8 initial-text))
  (define init-len (bytes-length init-bs))
  (define cap (max 256 (* 2 init-len)))
  (define bs (make-bytes cap 0))
  (define fs (make-bytes cap 0))
  (bytes-copy! bs 0 init-bs)
  ;; faces stay 0 — face.rkt / colorer fills them
  (define gb (gap-buffer bs fs init-len cap))
  (check-gap-invariant! gb 'make-gap-buffer)
  gb)

;; ============================================================
;; Invariant Checker
;; ============================================================

(define (check-gap-invariant! gb [context 'gap])
  (define bs  (gap-buffer-bytes gb))
  (define fs  (gap-buffer-faces gb))
  (define gs  (gap-buffer-gap-start gb))
  (define ge  (gap-buffer-gap-end gb))
  (define blen (bytes-length bs))
  (define flen (bytes-length fs))

  (unless (and (>= gs 0) (<= gs ge) (<= ge blen))
    (error context (format "I1: 0≤gs(~a)≤ge(~a)≤blen(~a)" gs ge blen)))
  (unless (= blen flen)
    (error context (format "I2: bytes-len(~a)=faces-len(~a)" blen flen)))
  ;; I4: gap area in faces is zero-filled
  (for ([i (in-range gs ge)])
    (unless (zero? (bytes-ref fs i))
      (error context (format "I4: faces[~a]=~a (expected 0 in gap)" i (bytes-ref fs i)))))
  (void))

;; ============================================================
;; Pure: Index Mapping
;; ============================================================

(define (logical-index gb physical-pos)
  (define gs (gap-buffer-gap-start gb))
  (define ge (gap-buffer-gap-end gb))
  (cond [(< physical-pos gs)   physical-pos]
        [(>= physical-pos ge)  (- physical-pos (- ge gs))]
        [else                  gs]))

(define (physical-index gb logical-pos)
  (if (< logical-pos (gap-buffer-gap-start gb))
      logical-pos
      (+ logical-pos (- (gap-buffer-gap-end gb) (gap-buffer-gap-start gb)))))

;; ============================================================
;; Pure: Text Queries
;; ============================================================

(define (gap-length gb)
  (- (bytes-length (gap-buffer-bytes gb))
     (- (gap-buffer-gap-end gb) (gap-buffer-gap-start gb))))

(define (gap-valid-pos? gb pos)
  (and (exact-integer? pos) (>= pos 0) (<= pos (gap-length gb))))

(define (gap-valid-range? gb from to)
  (and (exact-integer? from) (exact-integer? to)
       (>= from 0) (<= to (gap-length gb)) (<= from to)))

(define (gap-byte-ref gb byte-pos)
  (unless (and (gap-valid-pos? gb byte-pos) (< byte-pos (gap-length gb)))
    (raise-argument-error 'gap-byte-ref
                          (format "valid pos [0, ~a)" (gap-length gb)) byte-pos))
  (bytes-ref (gap-buffer-bytes gb) (physical-index gb byte-pos)))

(define (gap-subbytes gb from to)
  (unless (gap-valid-range? gb from to)
    (raise-argument-error 'gap-subbytes
                          (format "valid range [0, ~a]" (gap-length gb)) (list from to)))
  (define gs (gap-buffer-gap-start gb))
  (define ge (gap-buffer-gap-end gb))
  (define bs (gap-buffer-bytes gb))
  (cond [(<= to gs)  (subbytes bs from to)]
        [(>= from gs) (subbytes bs (+ from (- ge gs)) (+ to (- ge gs)))]
        [else (bytes-append (subbytes bs from gs)
                            (subbytes bs ge (+ to (- ge gs))))]))

;; ============================================================
;; Internal: gap-move-to!
;; ============================================================
;; Repositions the gap.  Moves face data in lockstep with text,
;; then zero-fills the new gap area in faces.

(define (gap-move-to! gb byte-pos)
  (define gs (gap-buffer-gap-start gb))
  (define ge (gap-buffer-gap-end gb))
  (define bs (gap-buffer-bytes gb))
  (define fs (gap-buffer-faces gb))

  (when (not (= byte-pos gs))
    (cond
      [(< byte-pos gs)
       (define shift (- gs byte-pos))
       (define new-ge (- ge shift))
       (bytes-copy! bs new-ge bs byte-pos gs)
       (bytes-copy! fs new-ge fs byte-pos gs)
       (for ([i (in-range byte-pos new-ge)]) (bytes-set! fs i 0))
       (set-gap-buffer-gap-start! gb byte-pos)
       (set-gap-buffer-gap-end! gb new-ge)]
      [(> byte-pos gs)
       (define shift (- byte-pos gs))
       (define new-ge (+ ge shift))
       (bytes-copy! bs gs bs ge new-ge)
       (bytes-copy! fs gs fs ge new-ge)
       (for ([i (in-range gs new-ge)]) (bytes-set! fs i 0))
       (set-gap-buffer-gap-start! gb byte-pos)
       (set-gap-buffer-gap-end! gb new-ge)]))
  (check-gap-invariant! gb 'gap-move-to!))

;; ============================================================
;; Internal: gap-reserve!
;; ============================================================
;; Ensures the gap has at least `need` free bytes.  Grows both
;; bytes and faces arrays if necessary.

(define (gap-reserve! gb need)
  (define gs (gap-buffer-gap-start gb))
  (define ge (gap-buffer-gap-end gb))
  (define gap-size (- ge gs))
  (when (< gap-size need)
    (define old-bs (gap-buffer-bytes gb))
    (define old-fs (gap-buffer-faces gb))
    (define old-len (bytes-length old-bs))
    (define new-cap (max (* 2 old-len) (+ old-len need)))
    (define new-bs (make-bytes new-cap 0))
    (define new-fs (make-bytes new-cap 0))
    (bytes-copy! new-bs 0 old-bs 0 gs)
    (bytes-copy! new-fs 0 old-fs 0 gs)
    (define new-ge (- new-cap (- old-len ge)))
    (bytes-copy! new-bs new-ge old-bs ge old-len)
    (bytes-copy! new-fs new-ge old-fs ge old-len)
    (set-gap-buffer-bytes! gb new-bs)
    (set-gap-buffer-faces! gb new-fs)
    (set-gap-buffer-gap-end! gb new-ge))
  (check-gap-invariant! gb 'gap-reserve!))

;; ============================================================
;; Public: gap-insert!
;; ============================================================
;; Insert raw bytes at logical byte-pos.  Face-ids default to 0.

(define (gap-insert! gb byte-pos bs)
  (unless (bytes? bs)
    (raise-argument-error 'gap-insert! "bytes?" bs))
  (unless (gap-valid-pos? gb byte-pos)
    (raise-argument-error 'gap-insert!
                          (format "valid pos [0, ~a]" (gap-length gb)) byte-pos))
  (define blen (bytes-length bs))
  (when (positive? blen)
    (gap-move-to! gb byte-pos)
    (gap-reserve! gb blen)
    (define gs (gap-buffer-gap-start gb))
    (bytes-copy! (gap-buffer-bytes gb) gs bs)
    ;; New face slots default to 0 (already zero from gap-reserve!)
    (set-gap-buffer-gap-start! gb (+ gs blen))
    (check-gap-invariant! gb 'gap-insert!)))

;; ============================================================
;; Public: gap-delete!
;; ============================================================
;; Delete bytes in logical range [from, to).  Returns void.

(define (gap-delete! gb from to)
  (unless (gap-valid-range? gb from to)
    (raise-argument-error 'gap-delete!
                          (format "valid range [0, ~a]" (gap-length gb)) (list from to)))
  (define count (- to from))
  (when (positive? count)
    (gap-move-to! gb from)
    (define old-ge (gap-buffer-gap-end gb))
    (set-gap-buffer-gap-end! gb (+ old-ge count))
    ;; Zero-fill the expanded gap in faces
    (define fs (gap-buffer-faces gb))
    (for ([i (in-range old-ge (+ old-ge count))]) (bytes-set! fs i 0))
    (check-gap-invariant! gb 'gap-delete!)))
