#lang racket

;; kernel/buffer.rkt — Buffer kernel primitives
;;
;; Minimal: buffer struct, insert/delete with hooks+undo, point, mark, region.

(require "gap.rkt"
         "marker.rkt"
         "undo.rkt")

(provide
 ;; struct
 make-buffer buffer?
 buffer-name buffer-gap buffer-point-marker
 buffer-mark buffer-markers
 buffer-modified? buffer-modiff
 buffer-filename buffer-begv buffer-zv
 buffer-read-only? buffer-saved-modiff
 set-buffer-mark! set-buffer-markers!
 set-buffer-modified?! set-buffer-modiff!
 set-buffer-filename! set-buffer-begv! set-buffer-zv!
 set-buffer-read-only?! set-buffer-saved-modiff!
 set-buffer-name!

 ;; hooks
 buffer-hooks buffer-undo-rec
 hook-manager-before-fns hook-manager-after-fns
 set-hook-manager-before-fns! set-hook-manager-after-fns!
 default-before-change-functions default-after-change-functions

 ;; locals
 buffer-var set-buffer-var! kill-buffer-var!
 truncate-lines? set-truncate-lines?!
 buffer-cleanup!
 set-buffer-mode-name! buffer-mode-name

 ;; change tracking
 buffer-change-region clear-buffer-change-region!

 ;; operations
 buffer-insert buffer-delete buffer-undo buffer-redo
 buffer-point set-buffer-point!
 buffer-byte-length buffer-string buffer-substring
 buffer-char-at

 ;; mark / region
 set-mark region-active? region-beginning region-end

 ;; current buffer
 current-buffer set-buffer

 ;; undo boundary
 undo-recorder-push-boundary!)

;; ============================================================
;; Buffer struct
;; ============================================================

(struct buffer
  ([name #:mutable]
   gap
   point-marker
   [mark #:mutable]
   [markers #:mutable]
   [modified? #:mutable]
   [modiff #:mutable]
   [filename #:mutable]
   [begv #:mutable]
   [zv #:mutable]
   [read-only? #:mutable]
   [saved-modiff #:mutable])
  #:transparent)

;; ============================================================
;; Subsystem tables
;; ============================================================

(define hooks-table       (make-hasheq))
(define undo-table        (make-hasheq))
(define insert-proc-table (make-hasheq))
(define delete-proc-table (make-hasheq))
(define undo-proc-table   (make-hasheq))
(define redo-proc-table   (make-hasheq))
(define local-table       (make-hasheq))
(define change-table      (make-hasheq))

(define (buffer-hooks buf) (hash-ref hooks-table buf (λ () (error 'buffer-hooks "dead buffer"))))
(define (buffer-undo-rec buf) (hash-ref undo-table buf (λ () (error 'buffer-undo-rec "dead buffer"))))
(define (get-insert-proc buf) (hash-ref insert-proc-table buf (λ () (error 'buffer-insert "dead buffer"))))
(define (get-delete-proc buf) (hash-ref delete-proc-table buf (λ () (error 'buffer-delete "dead buffer"))))
(define (get-undo-proc buf) (hash-ref undo-proc-table buf (λ () (error 'buffer-undo "dead buffer"))))
(define (get-redo-proc buf) (hash-ref redo-proc-table buf (λ () (error 'buffer-redo "dead buffer"))))

;; ============================================================
;; Hook manager
;; ============================================================

(struct hook-manager
  ([before-fns #:mutable] [after-fns #:mutable]) #:transparent)

(define (make-hook-manager) (hook-manager '() '()))
(define default-before-change-functions (make-parameter '()))
(define default-after-change-functions  (make-parameter '()))

;; ============================================================
;; Buffer-local variables
;; ============================================================

(define (buffer-var buf sym [default #f])
  (define tbl (hash-ref local-table buf (λ () (make-hasheq))))
  (hash-ref tbl sym (λ () default)))
(define (set-buffer-var! buf sym value)
  (define tbl (hash-ref! local-table buf make-hasheq))
  (hash-set! tbl sym value))
(define (kill-buffer-var! buf sym)
  (define tbl (hash-ref local-table buf (λ () (make-hasheq))))
  (hash-remove! tbl sym))

(define (truncate-lines? [buf (current-buffer)]) (buffer-var buf 'truncate-lines #t))
(define (set-truncate-lines?! v [buf (current-buffer)]) (set-buffer-var! buf 'truncate-lines v))

;; ============================================================
;; Change tracking
;; ============================================================

(define (buffer-change-region buf) (hash-ref change-table buf (λ () #f)))
(define (clear-buffer-change-region! buf) (hash-set! change-table buf #f))

;; ============================================================
;; make-buffer
;; ============================================================

(define (make-buffer name [initial-text ""] [filename #f])
  (define gb (make-gap-buffer initial-text))
  (define init-len (gap-byte-length gb))
  (define pt (make-marker init-len #t #f))
  (define hm (make-hook-manager))
  (for ([f (in-list (default-before-change-functions))])
    (set-hook-manager-before-fns! hm (append (hook-manager-before-fns hm) (list f))))
  (for ([f (in-list (default-after-change-functions))])
    (set-hook-manager-after-fns! hm (append (hook-manager-after-fns hm) (list f))))
  (define undo (make-undo-recorder))
  (define markers (list pt))
  (define buf (buffer name gb pt #f markers #f 0 filename 0 init-len #f 0))
  (set-marker-buffer! pt buf)
  (hash-set! hooks-table buf hm)
  (hash-set! undo-table  buf undo)

  (define (mark-modified! change-start change-end)
    (set-buffer-modified?! buf #t)
    (set-buffer-modiff! buf (add1 (buffer-modiff buf)))
    (define prev (hash-ref change-table buf (λ () #f)))
    (hash-set! change-table buf
               (if prev (cons (min (car prev) change-start) (max (cdr prev) change-end))
                   (cons change-start change-end))))

  (define (raw-insert! str byte-pos)
    (define bs (string->bytes/utf-8 str))
    (define blen (bytes-length bs))
    (gap-insert-bytes! gb byte-pos bs)
    (adjust-markers-insert! (buffer-markers buf) byte-pos blen))

  (define (raw-delete! from to)
    (gap-delete-range! gb from to)
    (adjust-markers-delete! (buffer-markers buf) from to))

  (define (insert! str #:at byte-pos)
    (define bs (string->bytes/utf-8 str))
    (define blen (bytes-length bs))
    (when (positive? blen)
      (when (buffer-read-only? buf) (error 'buffer-insert "read-only"))
      (undo-recorder-record-insert! undo byte-pos blen)
      (for ([f (in-list (hook-manager-before-fns hm))]) (f buf byte-pos byte-pos))
      (raw-insert! str byte-pos)
      (set-buffer-zv! buf (gap-byte-length gb))
      (for ([f (in-list (hook-manager-after-fns hm))]) (f buf byte-pos 0 blen))
      (mark-modified! byte-pos (+ byte-pos blen))))

  (define (delete! from to)
    (when (< from to)
      (when (buffer-read-only? buf) (error 'buffer-delete "read-only"))
      (define text (gap-substring gb from to))
      (define blen (- to from))
      (undo-recorder-record-delete! undo from text (= (marker-pos pt) to))
      (for ([f (in-list (hook-manager-before-fns hm))]) (f buf from to))
      (raw-delete! from to)
      (set-buffer-zv! buf (gap-byte-length gb))
      (for ([f (in-list (hook-manager-after-fns hm))]) (f buf from blen 0))
      (mark-modified! from from)))

  (define (undo!)
    (let/ec return
      (when (null? (undo-recorder-undo-stack undo)) (return #f))
      (define group (car (undo-recorder-undo-stack undo)))
      (set-undo-recorder-undo-stack! undo (cdr (undo-recorder-undo-stack undo)))
      (for ([rec (in-list (undo-group-records group))])
        (cond [(undo-insert? rec) (delete! (undo-insert-beg rec) (undo-insert-end rec))
               (set-marker-pos! pt (undo-insert-beg rec))]
              [(undo-delete? rec)
               (define text (undo-delete-text rec)) (define beg (undo-delete-beg rec))
               (define target (if (undo-delete-pt-at-end? rec)
                                  (+ beg (bytes-length (string->bytes/utf-8 text))) beg))
               (insert! text #:at beg) (set-marker-pos! pt target)]))
      (set-undo-recorder-redo-stack! undo (cons group (undo-recorder-redo-stack undo)))
      #t))

  (define (redo!)
    (when (null? (undo-recorder-redo-stack undo)) #f)
    (define group (car (undo-recorder-redo-stack undo)))
    (set-undo-recorder-redo-stack! undo (cdr (undo-recorder-redo-stack undo)))
    (for ([rec (in-list (undo-group-records group))])
      (cond [(undo-insert? rec) (delete! (undo-insert-beg rec) (undo-insert-end rec))
             (set-marker-pos! pt (undo-insert-beg rec))]
            [(undo-delete? rec)
             (define text (undo-delete-text rec)) (define beg (undo-delete-beg rec))
             (define target (if (undo-delete-pt-at-end? rec)
                                (+ beg (bytes-length (string->bytes/utf-8 text))) beg))
             (insert! text #:at beg) (set-marker-pos! pt target)]))
    (set-undo-recorder-undo-stack! undo (cons group (undo-recorder-undo-stack undo)))
    #t)

  (hash-set! insert-proc-table buf insert!)
  (hash-set! delete-proc-table buf delete!)
  (hash-set! undo-proc-table   buf undo!)
  (hash-set! redo-proc-table   buf redo!)
  buf)

;; ============================================================
;; Public operations
;; ============================================================

(define (buffer-insert buf str #:at [byte-pos (buffer-point buf)])
  ((get-insert-proc buf) str #:at byte-pos))
(define (buffer-delete buf from to)
  ((get-delete-proc buf) from to))
(define (buffer-undo buf) ((get-undo-proc buf)))
(define (buffer-redo buf) ((get-redo-proc buf)))
(define (buffer-point buf) (marker-pos (buffer-point-marker buf)))
(define (set-buffer-point! buf pos) (set-marker-pos! (buffer-point-marker buf) pos))
(define (buffer-byte-length buf) (gap-byte-length (buffer-gap buf)))
(define (buffer-substring buf from to) (gap-substring (buffer-gap buf) from to))
(define (buffer-string buf) (buffer-substring buf 0 (buffer-byte-length buf)))
(define (buffer-char-at buf pos) (let-values ([(ch l) (gap-char-at (buffer-gap buf) pos)]) ch))

;; ============================================================
;; Current buffer
;; ============================================================

(define current-buffer (make-parameter #f))
(define (set-buffer buf) (current-buffer buf))

;; ============================================================
;; Mark / Region
;; ============================================================

(define (set-mark #:buf [b (current-buffer)])
  (define m (make-marker (buffer-point b) #f))
  (set-buffer-mark! b m)
  (set-buffer-markers! b (cons m (buffer-markers b))))

(define (region-active? #:buf [b (current-buffer)])
  (define m (buffer-mark b))
  (and m (not (= (marker-pos m) (buffer-point b)))))

(define (region-beginning #:buf [b (current-buffer)])
  (define m (buffer-mark b))
  (if m (min (buffer-point b) (marker-pos m)) (buffer-point b)))

(define (region-end #:buf [b (current-buffer)])
  (define m (buffer-mark b))
  (if m (max (buffer-point b) (marker-pos m)) (buffer-point b)))

;; For base/registry kill-buffer
(define (buffer-cleanup! buf)
  (for ([t (list hooks-table undo-table insert-proc-table delete-proc-table
                 undo-proc-table redo-proc-table local-table change-table)])
    (hash-remove! t buf)))

;; ── mode-name convenience (backed by buffer-var) ──
(define (set-buffer-mode-name! buf name) (set-buffer-var! buf 'mode-name name))
(define (buffer-mode-name buf) (buffer-var buf 'mode-name 'Fundamental))
