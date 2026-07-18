#lang racket

;; kernel/buffer.rkt — Buffer: text + undo + point/mark + text-properties
;;
;; Composes text.rkt, textprop.rkt, undo/* into the core editor buffer.
;;
;; This module is pure kernel — NO display, NO dirty flags, NO rendering.
;; The buffer knows about text mutation, undo history, point/mark.
;;
;; Architecture:
;;   gap-buffer (bytes)  ←  gap.rkt
;;   markers             ←  marker.rkt
;;   text                ←  gap + marker list  (text.rkt)
;;   text-properties     ←  interval-map       (textprop.rkt)
;;   undo                ←  record/recorder/exec
;;   buffer              ←  text + undo + point/mark + text-props
;;
;; All mutations return change information as values:
;;   (values start end) — the byte range affected in the buffer.
;;   The caller composes these freely; kernel never tracks dirty state.

(require "data/text.rkt"
         "data/query.rkt"
         "data/textprop.rkt"
         "undo/record.rkt"
         "undo/recorder.rkt"
         "undo/exec.rkt")

(provide
 ;; struct + constructor
 buffer? make-buffer
 buffer-name buffer-text buffer-text-props buffer-undo-recorder
 buffer-point-marker buffer-mark-marker
 buffer-modified? buffer-modiff buffer-filename
 buffer-read-only? buffer-saved-modiff
 set-buffer-name! set-buffer-modified?! set-buffer-modiff!
 set-buffer-filename! set-buffer-read-only?! set-buffer-saved-modiff!

 ;; mutations — all return (values start end)
 buffer-insert! buffer-delete! buffer-undo! buffer-redo!

 ;; point
 buffer-point set-buffer-point!

 ;; mark / region
 set-mark! buffer-mark region-active? region-beginning region-end
 deactivate-mark!

 ;; queries
 buffer-length buffer-substring buffer-string

 ;; text properties
 buffer-face-at
 buffer-prop-get buffer-prop-put! buffer-prop-remove!)

;; ============================================================
;; Struct
;; ============================================================

(struct buffer
  ([name #:mutable]
   text                 ; text? — underlying gap+markers
   [text-props #:mutable]  ; text-properties? — faces + metadata
   undo-recorder        ; undo-recorder? — edit history
   point-marker         ; marker? — main cursor
   [mark-marker #:mutable]  ; (or/c marker? #f) — region anchor
   [modified? #:mutable]
   [modiff #:mutable]
   [filename #:mutable]
   [read-only? #:mutable]
   [saved-modiff #:mutable])
  #:transparent)

(define (make-buffer [name "*scratch*"] [initial ""] [filename #f])
  (define tx (make-text initial))
  (define tp (make-text-properties))
  (define pt (text-marker! tx 0 #t)) ; insertion-type = stay after inserts
  (define rec (make-undo-recorder))
  (buffer name tx tp rec pt #f #f 0 filename #f 0))

;; ============================================================
;; Mutations — return (values start end) change extent
;; ============================================================

(define (buffer-insert! buf str byte-pos)
  ;; Returns (values byte-pos byte-pos+blen) — the inserted range.
  (define bs (string->bytes/utf-8 str))
  (define blen (bytes-length bs))
  (if (positive? blen)
      (begin
        (text-insert! (buffer-text buf) byte-pos bs)
        (textprop-adjust-insert! (buffer-text-props buf) byte-pos blen)
        (recorder-record-insert! (buffer-undo-recorder buf) byte-pos
                                 (+ byte-pos blen))
        (set-buffer-modified?! buf #t)
        (set-buffer-modiff! buf (add1 (buffer-modiff buf)))
        (values byte-pos (+ byte-pos blen)))
      ;; No change
      (values byte-pos byte-pos)))

(define (buffer-delete! buf from to)
  ;; Returns (values from from) — position of the deletion.
  (if (< from to)
      (let ([text-str (buffer-substring buf from to)])
        (text-delete! (buffer-text buf) from to)
        (textprop-adjust-delete! (buffer-text-props buf) from to)
        (recorder-record-delete! (buffer-undo-recorder buf) text-str from)
        (set-buffer-modified?! buf #t)
        (set-buffer-modiff! buf (add1 (buffer-modiff buf)))
        (values from from))
      ;; No change
      (values from from)))

;; ============================================================
;; Undo / Redo — return (values start end) or (values #f #f)
;; ============================================================

(define (buffer-undo! buf)
  ;; Returns (values start end) of the undone change range,
  ;; clamped to current buffer length, or (values #f #f).
  (define rec (buffer-undo-recorder buf))
  (if (pair? (undo-recorder-undo-stack rec))
      (let* ([group (car (undo-recorder-undo-stack rec))]
             [tx (buffer-text buf)])
        ;; Capture text of inserts BEFORE undo deletes them (needed for redo)
        (for ([r (in-list (undo-group-records group))]
              #:when (undo-insert? r))
          (set-undo-insert-text! r
            (buffer-substring buf (undo-insert-beg r) (undo-insert-end r))))
        (set-undo-recorder-undo-stack! rec
          (cdr (undo-recorder-undo-stack rec)))
        (execute-undo! tx group)
        (restore-point-after-undo! buf group)
        (set-undo-recorder-redo-stack! rec
          (cons group (undo-recorder-redo-stack rec)))
        (set-buffer-modified?! buf #t)
        (set-buffer-modiff! buf (add1 (buffer-modiff buf)))
        (define buflen (buffer-length buf))
        (define raw (undo-group-range group))
        (values (min (car raw) buflen) (min (cdr raw) buflen)))
      (values #f #f)))

(define (buffer-redo! buf)
  ;; Returns (values start end) of the redone change range,
  ;; clamped to current buffer length, or (values #f #f).
  (define rec (buffer-undo-recorder buf))
  (if (pair? (undo-recorder-redo-stack rec))
      (let* ([group (car (undo-recorder-redo-stack rec))]
             [tx (buffer-text buf)])
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

;; ============================================================
;; undo-group-range — compute the byte range affected by a group
;; ============================================================

(define (undo-group-range group)
  ;; Returns (cons start end).  For use as return value of undo/redo.
  (define records (undo-group-records group))
  (if (null? records)
      (cons 0 0)
      (let loop ([rs records] [mn +inf.0] [mx -inf.0])
        (if (null? rs)
            (cons (if (= mn +inf.0) 0 mn) (if (= mx -inf.0) 0 mx))
            (let ([r (car rs)])
              (cond [(undo-insert? r)
                     (loop (cdr rs)
                           (min mn (undo-insert-beg r))
                           (max mx (undo-insert-end r)))]
                    [(undo-delete? r)
                     (define blen (bytes-length
                                   (string->bytes/utf-8 (undo-delete-text r))))
                     (loop (cdr rs)
                           (min mn (undo-delete-beg r))
                           (max mx (+ (undo-delete-beg r) blen)))]))))))

(define (restore-point-after-undo! buf group)
  (define records (undo-group-records group))
  (when (pair? records)
    (define first (car records))
    (cond [(undo-insert? first)
           (set-buffer-point! buf (undo-insert-beg first))]
          [(undo-delete? first)
           (define text (undo-delete-text first))
           (define blen (bytes-length (string->bytes/utf-8 text)))
           (set-buffer-point! buf (+ (undo-delete-beg first) blen))])))

;; ============================================================
;; Point
;; ============================================================

(define (buffer-point buf)
  (text-marker-pos (buffer-text buf) (buffer-point-marker buf)))

(define (set-buffer-point! buf pos)
  ;; Clamp to [0, buflen] then snap to nearest valid UTF-8 character start.
  ;; Prevents cursor from landing in the middle of a multi-byte character.
  (define clamped (max 0 (min pos (buffer-length buf))))
  (define gb (text-gap (buffer-text buf)))
  (define safe (gap-char-start gb clamped))
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

(define (buffer-mark buf)
  (buffer-mark-marker buf))

(define (region-active? buf)
  (define m (buffer-mark-marker buf))
  (and m (not (= (text-marker-pos (buffer-text buf) m)
                 (buffer-point buf)))))

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

(define (buffer-length buf)
  (text-length (buffer-text buf)))

(define (buffer-substring buf from to)
  (define real-from (max 0 from))
  (define real-to (min to (buffer-length buf)))
  (if (< real-from real-to)
      (bytes->string/utf-8 (text-subbytes (buffer-text buf) real-from real-to))
      ""))

(define (buffer-string buf)
  (buffer-substring buf 0 (buffer-length buf)))

;; ============================================================
;; Text properties
;; ============================================================

(define (buffer-face-at buf pos)
  (textprop-face-at (buffer-text-props buf) pos))

(define (buffer-prop-get buf pos key [default #f])
  (textprop-get (buffer-text-props buf) pos key default))

(define (buffer-prop-put! buf from to key value)
  (textprop-put! (buffer-text-props buf) from to key value))

(define (buffer-prop-remove! buf from to)
  (textprop-remove! (buffer-text-props buf) from to))
