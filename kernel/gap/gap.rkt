#lang racket

;; kernel/gap/gap.rkt — Bytes-Based Gap Buffer
;;
;; Layout:  [text-before]  [gap]  [text-after]
;;          ↑ 0         ↑ gap-start  ↑ gap-end  ↑ bytes-length
;;
;; All positions are byte offsets into the logical UTF-8 byte stream.
;; This module is byte-level only — no character decoding, no scanning.
;; Those live in utf8.rkt and query.rkt.
;;
;; Public interface (7 exports):
;;   make-gap-buffer  gap-buffer?  gap-bytes  gap-length
;;   gap-byte-ref  gap-subbytes    (queries)
;;   gap-insert!   gap-delete!     (mutations)

(provide
 ;; constructor + predicate
 make-gap-buffer gap-buffer?

 ;; accessors (struct fields are public for query.rkt)
 gap-buffer-bytes gap-buffer-gap-start gap-buffer-gap-end

 ;; queries
 gap-length
 gap-byte-ref
 gap-subbytes

 ;; mutations
 gap-insert!
 gap-delete!)

;; ============================================================
;; Struct
;; ============================================================

(struct gap-buffer
  ([bytes #:mutable]     ; bytes? — includes gap region
   [gap-start #:mutable] ; byte-pos — first byte of gap
   [gap-end #:mutable])  ; byte-pos — first byte after gap
  #:transparent)

;; ============================================================
;; Constructor
;; ============================================================

(define (make-gap-buffer [initial-text ""])
  (define init-bs (string->bytes/utf-8 initial-text))
  (define init-len (bytes-length init-bs))
  (define cap (max 256 (* 2 init-len)))
  (define bs (make-bytes cap 0))
  (bytes-copy! bs 0 init-bs)
  (gap-buffer bs init-len cap))

;; ============================================================
;; Queries
;; ============================================================

(define (gap-length gb)
  ;; Total logical byte count (gap excluded).
  (- (bytes-length (gap-buffer-bytes gb))
     (- (gap-buffer-gap-end gb) (gap-buffer-gap-start gb))))

(define (gap-byte-ref gb byte-pos)
  ;; O(1) single byte at logical byte-pos.
  (bytes-ref (gap-buffer-bytes gb) (physical-index gb byte-pos)))

(define (gap-subbytes gb from to)
  ;; Raw bytes in logical range [from, to). Handles gap crossing.
  ;; Used by query.rkt to build substrings without knowing gap internals.
  (define gs (gap-buffer-gap-start gb))
  (define ge (gap-buffer-gap-end gb))
  (define len (gap-length gb))
  (define real-to (min to len))
  (define bs (gap-buffer-bytes gb))
  (cond
    [(<= real-to gs)
     ;; Entirely before gap
     (subbytes bs from real-to)]
    [(>= from gs)
     ;; Entirely after gap
     (subbytes bs (+ from (- ge gs)) (+ real-to (- ge gs)))]
    [else
     ;; Spans the gap
     (bytes-append
      (subbytes bs from gs)
      (subbytes bs ge (+ real-to (- ge gs))))]))

;; ============================================================
;; Internal: physical index mapping
;; ============================================================

(define (physical-index gb logical-pos)
  (if (< logical-pos (gap-buffer-gap-start gb))
      logical-pos
      (+ logical-pos (- (gap-buffer-gap-end gb) (gap-buffer-gap-start gb)))))

;; ============================================================
;; Internal: gap-move-to!
;; ============================================================

(define (gap-move-to! gb byte-pos)
  (define gs (gap-buffer-gap-start gb))
  (define ge (gap-buffer-gap-end gb))
  (cond
    [(< byte-pos gs)
     ;; Move gap left: copy [byte-pos, gs) to end of gap area
     (define shift (- gs byte-pos))
     (define bs (gap-buffer-bytes gb))
     (define new-ge (- ge shift))
     (bytes-copy! bs new-ge bs byte-pos gs)
     (set-gap-buffer-gap-start! gb byte-pos)
     (set-gap-buffer-gap-end! gb new-ge)]
    [(> byte-pos gs)
     ;; Move gap right: copy [ge, ge+shift) to gap-start
     (define shift (- byte-pos gs))
     (define bs (gap-buffer-bytes gb))
     (define new-ge (+ ge shift))
     (bytes-copy! bs gs bs ge new-ge)
     (set-gap-buffer-gap-start! gb byte-pos)
     (set-gap-buffer-gap-end! gb new-ge)]
    [else (void)]))

;; ============================================================
;; Internal: gap-reserve!
;; ============================================================

(define (gap-reserve! gb need)
  (define gs (gap-buffer-gap-start gb))
  (define ge (gap-buffer-gap-end gb))
  (define gap-size (- ge gs))
  (when (< gap-size need)
    (define old-bs (gap-buffer-bytes gb))
    (define old-len (bytes-length old-bs))
    (define new-cap (max (* 2 old-len) (+ old-len need)))
    (define new-bs (make-bytes new-cap 0))
    ;; Copy text before gap
    (bytes-copy! new-bs 0 old-bs 0 gs)
    ;; Copy text after gap (to end of new buffer)
    (define new-ge (- new-cap (- old-len ge)))
    (bytes-copy! new-bs new-ge old-bs ge old-len)
    (set-gap-buffer-bytes! gb new-bs)
    (set-gap-buffer-gap-end! gb new-ge)))

;; ============================================================
;; Mutations — only these two modify the gap
;; ============================================================

(define (gap-insert! gb byte-pos bs)
  ;; Insert raw bytes at logical byte-pos.
  ;; byte-pos must be a valid UTF-8 start byte (caller's responsibility).
  (define blen (bytes-length bs))
  (when (positive? blen)
    (gap-move-to! gb byte-pos)
    (gap-reserve! gb blen)
    (define gs (gap-buffer-gap-start gb))
    (define buf (gap-buffer-bytes gb))
    (bytes-copy! buf gs bs)
    (set-gap-buffer-gap-start! gb (+ gs blen))))

(define (gap-delete! gb from to)
  ;; Delete bytes in logical range [from, to).
  (define count (- to from))
  (when (positive? count)
    (gap-move-to! gb from)
    (set-gap-buffer-gap-end! gb (+ (gap-buffer-gap-end gb) count))))
