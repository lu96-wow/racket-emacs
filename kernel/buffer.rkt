#lang racket

;; kernel/buffer.rkt — Buffer: text + faces + undo + point/mark
;;
;; ============================================================================
;; Composes text.rkt (gap+markers) with face management and undo history.
;; Faces are stored in the gap buffer (colocated with text), but text.rkt
;; doesn't know about faces — buffer.rkt composes text ops + face ops.
;;
;; ============================================================================
;; Layer Responsibilities
;; ============================================================================
;;
;;   gap.rkt        — bytes + face array (internal), move/resize/grow
;;   text.rkt       — gap + markers, insert/delete bytes only
;;   buffer.rkt     — text + face capture/restore + undo + point/mark
;;   dirty.rkt      — buffer + change tracking for colorer
;;
;; ============================================================================
;; Face Management
;; ============================================================================
;;
;;   buffer-insert!   : text-insert! (faces default to 0 → colorer fills)
;;   buffer-delete!    : face-slice + text-delete! (faces captured for undo)
;;   buffer-face-at    : face-ref (O(1))
;;   buffer-face-fill! : face-fill! (used by colorer)
;;
;; Undo preserves faces:
;;   undo of delete → text-insert! then face-fill! from stored faces
;;   undo of insert → text-delete! (faces gone, no restore needed)
;;   redo of insert → text-insert! then face-fill! from captured faces
;;
;; ============================================================================

(require "data/text.rkt"
         "data/query.rkt"
         "data/face.rkt"
         "undo/record.rkt"
         "undo/recorder.rkt"
         "undo/exec.rkt")

(provide
 buffer? make-buffer
 buffer-name buffer-text buffer-undo-recorder
 buffer-point-marker buffer-mark-marker
 buffer-modified? buffer-modiff buffer-filename
 buffer-read-only? buffer-saved-modiff
 set-buffer-name! set-buffer-modified?! set-buffer-modiff!
 set-buffer-filename! set-buffer-read-only?! set-buffer-saved-modiff!

 ;; content mutations
 buffer-insert! buffer-delete! buffer-undo! buffer-redo!

 ;; point
 buffer-point set-buffer-point!

 ;; mark / region
 set-mark! buffer-mark region-active? region-beginning region-end deactivate-mark!

 ;; queries
 buffer-length buffer-substring buffer-string

 ;; face operations
 buffer-face-ref buffer-face-fill!)

;; ============================================================
;; Struct
;; ============================================================

(struct buffer
  ([name #:mutable]
   text
   undo-recorder
   point-marker
   [mark-marker #:mutable]
   [modified? #:mutable]
   [modiff #:mutable]
   [filename #:mutable]
   [read-only? #:mutable]
   [saved-modiff #:mutable])
  #:transparent)

(define (make-buffer [name "*scratch*"] [initial ""] [filename #f])
  (unless (string? name)
    (raise-argument-error 'make-buffer "string?" name))
  (unless (string? initial)
    (raise-argument-error 'make-buffer "string?" initial))
  (define tx (make-text initial))
  (define pt (text-marker! tx 0 #t))
  (define rec (make-undo-recorder))
  (buffer name tx rec pt #f #f 0 filename #f 0))

;; ============================================================
;; Internal helpers
;; ============================================================

(define (buf-gap buf) (text-gap (buffer-text buf)))

;; ============================================================
;; Content Mutations
;; ============================================================

(define (buffer-insert! buf str byte-pos)
  ;; Insert text.  Faces default to 0 — colorer fills them later.
  ;; Returns (values start end) change extent.
  (unless (string? str)
    (raise-argument-error 'buffer-insert! "string?" str))

  (define bs (string->bytes/utf-8 str))
  (define blen (bytes-length bs))
  (define real-pos (max 0 (min byte-pos (buffer-length buf))))

  (if (or (buffer-read-only? buf) (zero? blen))
      (values real-pos real-pos)
      (begin
        ;; Text layer: insert bytes (faces auto-default to 0 in gap)
        (text-insert! (buffer-text buf) real-pos bs)
        ;; Undo layer: record the insert
        (recorder-record-insert! (buffer-undo-recorder buf)
                                 real-pos (+ real-pos blen))
        ;; Metadata
        (set-buffer-modified?! buf #t)
        (set-buffer-modiff! buf (add1 (buffer-modiff buf)))
        (values real-pos (+ real-pos blen)))))

(define (buffer-delete! buf from to)
  ;; Delete text.  Captures faces before deletion for undo.
  ;; Returns (values from from) change extent.
  (define max-to (buffer-length buf))
  (define real-from (max 0 from))
  (define real-to (min to max-to))

  (if (or (buffer-read-only? buf) (>= real-from real-to))
      (values real-from real-from)
      (let* ([text-str    (buffer-substring buf real-from real-to)]
             ;; Capture faces BEFORE deleting text
             [saved-faces (face-slice (buf-gap buf) real-from real-to)])
        ;; Text layer: delete bytes (faces auto-deleted in gap)
        (text-delete! (buffer-text buf) real-from real-to)
        ;; Undo layer: record delete WITH faces for restoration
        (recorder-record-delete! (buffer-undo-recorder buf)
                                 text-str saved-faces real-from)
        ;; Metadata
        (set-buffer-modified?! buf #t)
        (set-buffer-modiff! buf (add1 (buffer-modiff buf)))
        (values real-from real-from))))

;; ============================================================
;; Undo / Redo
;; ============================================================

(define (buffer-undo! buf)
  (define rec (buffer-undo-recorder buf))
  (if (pair? (undo-recorder-undo-stack rec))
      (let* ([group (car (undo-recorder-undo-stack rec))]
             [tx    (buffer-text buf)])
        ;; Capture text + faces of inserts BEFORE undo deletes them
        (for ([r (in-list (undo-group-records group))]
              #:when (undo-insert? r))
          (set-undo-insert-text! r
            (buffer-substring buf (undo-insert-beg r) (undo-insert-end r)))
          (set-undo-insert-faces! r
            (face-slice (buf-gap buf) (undo-insert-beg r) (undo-insert-end r))))
        ;; Pop from undo stack
        (set-undo-recorder-undo-stack! rec
          (cdr (undo-recorder-undo-stack rec)))
        ;; Execute undo on text
        (execute-undo! tx group)
        ;; Restore point
        (restore-point-after-undo! buf group)
        ;; Push to redo stack
        (set-undo-recorder-redo-stack! rec
          (cons group (undo-recorder-redo-stack rec)))
        ;; Metadata
        (set-buffer-modified?! buf #t)
        (set-buffer-modiff! buf (add1 (buffer-modiff buf)))
        ;; Return change extent
        (define buflen (buffer-length buf))
        (define raw (undo-group-range group))
        (values (min (car raw) buflen) (min (cdr raw) buflen)))
      (values #f #f)))

(define (buffer-redo! buf)
  (define rec (buffer-undo-recorder buf))
  (if (pair? (undo-recorder-redo-stack rec))
      (let* ([group (car (undo-recorder-redo-stack rec))]
             [tx    (buffer-text buf)])
        (set-undo-recorder-redo-stack! rec
          (cdr (undo-recorder-redo-stack rec)))
        (execute-redo! tx group)
        (set-undo-recorder-undo-stack! rec
          (cons group (undo-recorder-undo-stack rec)))
        (set-buffer-modified?! buf #t)
        (set-buffer-modiff! buf (add1 (buffer-modiff buf)))
        (define buflen (buffer-length buf))
        (define raw (undo-group-range group))
        (values (min (car raw) buflen) (min (cdr raw) buflen)))
      (values #f #f)))

(define (undo-group-range group)
  (define records (undo-group-records group))
  (if (null? records)
      (cons 0 0)
      ;; Use first record to seed min/max, then loop over the rest
      (let* ([r0 (car records)]
             [seed (cond [(undo-insert? r0)
                          (cons (undo-insert-beg r0) (undo-insert-end r0))]
                         [(undo-delete? r0)
                          (define blen (bytes-length (string->bytes/utf-8 (undo-delete-text r0))))
                          (cons (undo-delete-beg r0) (+ (undo-delete-beg r0) blen))]
                         [else (cons 0 0)])])
        (let loop ([rs (cdr records)] [mn (car seed)] [mx (cdr seed)])
          (if (null? rs)
              (cons mn mx)
              (let ([r (car rs)])
                (cond
                  [(undo-insert? r)
                   (loop (cdr rs) (min mn (undo-insert-beg r)) (max mx (undo-insert-end r)))]
                  [(undo-delete? r)
                   (define blen (bytes-length (string->bytes/utf-8 (undo-delete-text r))))
                   (loop (cdr rs) (min mn (undo-delete-beg r)) (max mx (+ (undo-delete-beg r) blen)))]
                  [else (loop (cdr rs) mn mx)])))))))

(define (restore-point-after-undo! buf group)
  (define records (undo-group-records group))
  (when (pair? records)
    (define first (car records))
    (cond
      [(undo-insert? first) (set-buffer-point! buf (undo-insert-beg first))]
      [(undo-delete? first)
       (define blen (bytes-length (string->bytes/utf-8 (undo-delete-text first))))
       (set-buffer-point! buf (+ (undo-delete-beg first) blen))])))

;; ============================================================
;; Point
;; ============================================================

(define (buffer-point buf)
  (text-marker-pos (buffer-text buf) (buffer-point-marker buf)))

(define (set-buffer-point! buf pos)
  (define clamped (max 0 (min pos (buffer-length buf))))
  (define safe (gap-char-start (buf-gap buf) clamped))
  (text-set-marker-pos! (buffer-text buf) (buffer-point-marker buf) safe))

;; ============================================================
;; Mark / Region
;; ============================================================

(define (set-mark! buf)
  (define m (buffer-mark-marker buf))
  (if m
      (text-set-marker-pos! (buffer-text buf) m (buffer-point buf))
      (let ([new-m (text-marker! (buffer-text buf) (buffer-point buf) #f)])
        (set-buffer-mark-marker! buf new-m))))

(define (buffer-mark buf) (buffer-mark-marker buf))

(define (region-active? buf)
  (define m (buffer-mark-marker buf))
  (and m (not (= (text-marker-pos (buffer-text buf) m) (buffer-point buf)))))

(define (region-beginning buf)
  (define m (buffer-mark-marker buf))
  (define pt (buffer-point buf))
  (if m (min pt (text-marker-pos (buffer-text buf) m)) pt))

(define (region-end buf)
  (define m (buffer-mark-marker buf))
  (define pt (buffer-point buf))
  (if m (max pt (text-marker-pos (buffer-text buf) m)) pt))

(define (deactivate-mark! buf)
  (set-buffer-mark-marker! buf #f))

;; ============================================================
;; Queries
;; ============================================================

(define (buffer-length buf) (text-length (buffer-text buf)))

(define (buffer-substring buf from to)
  (define real-from (max 0 from))
  (define real-to (min to (buffer-length buf)))
  (if (< real-from real-to)
      (bytes->string/utf-8 (text-subbytes (buffer-text buf) real-from real-to))
      ""))

(define (buffer-string buf)
  (buffer-substring buf 0 (buffer-length buf)))

;; ============================================================
;; Face Operations (composed on top of gap, not text)
;; ============================================================

(define (buffer-face-ref buf pos)
  ;; Face-id at byte position.  O(1).
  ;; 0 = default face, 1..255 = registered faces.
  (face-ref (buf-gap buf) pos))

(define (buffer-face-fill! buf from to face-id)
  ;; Set face-id over a byte range.  Used by colorers.
  (face-fill! (buf-gap buf) from to face-id))
