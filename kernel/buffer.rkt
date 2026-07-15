#lang racket

;; kernel/buffer.rkt — Buffer: text + undo + change-tracking + point/mark
;;
;; Composes kernel/text.rkt, kernel/undo/*, and protocol/undo-exec.rkt
;; into a full editor buffer.  No hooks, no display — those are
;; orchestrated explicitly by the command loop.

(require "text.rkt"
         "undo/record.rkt"
         "undo/recorder.rkt"
         "undo/exec.rkt")

(provide
 ;; struct + constructor
 buffer? make-buffer
 buffer-name buffer-text buffer-undo-recorder
 buffer-point-marker buffer-mark-marker
 buffer-modified? buffer-modiff buffer-filename
 buffer-read-only? buffer-saved-modiff
 set-buffer-name! set-buffer-modified?! set-buffer-modiff!
 set-buffer-filename! set-buffer-read-only?! set-buffer-saved-modiff!
 ;; mutations
 buffer-insert! buffer-delete! buffer-undo! buffer-redo!
 ;; point
 buffer-point set-buffer-point!
 ;; mark / region
 set-mark! buffer-mark region-active? region-beginning region-end deactivate-mark!
 ;; queries
 buffer-length buffer-substring buffer-string
 ;; change-tracking
 buffer-change-region clear-buffer-change-region!)

;; ============================================================
;; Struct
;; ============================================================

(struct buffer
  ([name #:mutable]
   text                 ; text? — the underlying gap+markers
   undo-recorder        ; undo-recorder? — edit history
   point-marker         ; marker? — main cursor
   [mark-marker #:mutable]  ; (or/c marker? #f) — region anchor
   [modified? #:mutable]
   [modiff #:mutable]
   [filename #:mutable]
   [read-only? #:mutable]
   [saved-modiff #:mutable]
   change-tracker)      ; (box/c (or/c #f (cons/c int int)))
  #:transparent)

(define (make-buffer [name "*scratch*"] [initial ""] [filename #f])
  (define tx (make-text initial))
  (define pt (text-marker! tx 0 #t)) ; insertion-type = stay after inserts
  (define rec (make-undo-recorder))
  (define tracker (box #f))
  (buffer name tx rec pt #f #f 0 filename #f 0 tracker))

;; ============================================================
;; Change-tracker helpers
;; ============================================================

(define (extend-change-region! buf start end)
  (define prev (buffer-change-region buf))
  (define new
    (if prev
        (cons (min (car prev) start) (max (cdr prev) end))
        (cons start end)))
  (set-box! (buffer-change-tracker buf) new))

(define (buffer-change-region buf)
  (unbox (buffer-change-tracker buf)))

(define (clear-buffer-change-region! buf)
  (set-box! (buffer-change-tracker buf) #f))

;; ============================================================
;; Mutations
;; ============================================================

(define (buffer-insert! buf str byte-pos)
  (when (buffer-read-only? buf)
    (error 'buffer-insert! "buffer is read-only: ~a" (buffer-name buf)))
  (define bs (string->bytes/utf-8 str))
  (define blen (bytes-length bs))
  (when (positive? blen)
    (define tx (buffer-text buf))
    (text-insert! tx byte-pos bs)
    (recorder-record-insert! (buffer-undo-recorder buf) byte-pos
                             (+ byte-pos blen))
    (mark-modified! buf byte-pos (+ byte-pos blen))))

(define (buffer-delete! buf from to)
  (when (buffer-read-only? buf)
    (error 'buffer-delete! "buffer is read-only: ~a" (buffer-name buf)))
  (define text-str (buffer-substring buf from to))
  (text-delete! (buffer-text buf) from to)
  (recorder-record-delete! (buffer-undo-recorder buf) text-str from)
  (mark-modified! buf from from))

(define (mark-modified! buf start end)
  (set-buffer-modified?! buf #t)
  (set-buffer-modiff! buf (add1 (buffer-modiff buf)))
  (extend-change-region! buf start end))

;; ============================================================
;; Undo / Redo
;; ============================================================

(define (buffer-undo! buf)
  (define rec (buffer-undo-recorder buf))
  (or (and (pair? (undo-recorder-undo-stack rec))
           (let* ([group (car (undo-recorder-undo-stack rec))]
                  [tx (buffer-text buf)])
             (set-undo-recorder-undo-stack! rec
               (cdr (undo-recorder-undo-stack rec)))
             (execute-undo! tx group)
             ;; Restore point to beginning of affected range
             (restore-point-after-undo! buf group)
             (set-undo-recorder-redo-stack! rec
               (cons group (undo-recorder-redo-stack rec)))
             #t))
      #f))

(define (buffer-redo! buf)
  (define rec (buffer-undo-recorder buf))
  (or (and (pair? (undo-recorder-redo-stack rec))
           (let* ([group (car (undo-recorder-redo-stack rec))]
                  [tx (buffer-text buf)])
             (set-undo-recorder-redo-stack! rec
               (cdr (undo-recorder-redo-stack rec)))
             (execute-redo! tx group)
             (set-undo-recorder-undo-stack! rec
               (cons group (undo-recorder-undo-stack rec)))
             #t))
      #f))

(define (restore-point-after-undo! buf group)
  ;; Put point at the beginning of the first record's affected range.
  (define records (undo-group-records group))
  (when (pair? records)
    (define first (car records))
    (cond [(undo-insert? first)
           (set-buffer-point! buf (undo-insert-beg first))]
          [(undo-delete? first)
           (set-buffer-point! buf (undo-delete-beg first))])))

;; ============================================================
;; Point
;; ============================================================

(define (buffer-point buf)
  (text-marker-pos (buffer-text buf) (buffer-point-marker buf)))

(define (set-buffer-point! buf pos)
  (text-set-marker-pos! (buffer-text buf) (buffer-point-marker buf)
                        (max 0 (min pos (buffer-length buf)))))

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
