#lang racket

;; display/layout.rkt — Visual-line generation and position mapping
;;
;; Pure layout: buffer bytes → display rows.  Two modes:
;;   truncate — one visual-line per logical line, '$' at col limit
;;   char     — split logical lines at max-cols, continued lines marked
;;
;; Dependencies: kernel/gap, display/char-width

(require "../kernel/gap/gap.rkt"
         "../kernel/gap/query.rkt"
         "char-width.rkt")

(provide
 ;; visual-line
 visual-line? visual-line-buf-pos visual-line-content
 visual-line-continued? visual-line-truncated? visual-line-display-len

 ;; line generation
 truncate-lines wrap-lines
 visual-line-lines

 ;; position mapping
 pos->row-col

 ;; per-buffer state (wrap mode, hscroll, current-buffer)
 buffer-wrap-mode set-buffer-wrap-mode!
 buffer-hscroll set-buffer-hscroll!
 truncate-lines?)

;; ============================================================
;; visual-line
;; ============================================================

(struct visual-line (buf-pos content continued? truncated? display-len)
  #:transparent)

;; ============================================================
;; truncate-lines — one visual-line per logical line, '$' for overflow
;; ============================================================

(define (truncate-lines gb start-pos max-rows max-cols left-col)
  (define len (gap-length gb))
  (define (nl? b) (= b #x0A))
  (let loop ([buf-pos start-pos] [row 0] [acc '()])
    (if (or (>= row max-rows) (>= buf-pos len))
        (reverse acc)
        (let* ([line-end    (gap-scan-byte gb buf-pos 'forward nl?)]
               [line-limit  (if (< line-end len) line-end len)]
               [full-width  (gap-display-width gb buf-pos line-limit)]
               [trunc?      (> full-width (+ left-col max-cols))]
               [reserve-$   (if trunc? 1 0)]
               [seg-start   (if (> left-col 0)
                                (scan-display-width gb buf-pos line-limit left-col)
                                buf-pos)]
               [actual-left (gap-display-width gb buf-pos seg-start)]
               [extra-cols  (- left-col actual-left)]
               [cols-left   (max 1 (- max-cols reserve-$ extra-cols))]
               [seg-end     (min line-limit
                                 (scan-display-width gb seg-start line-limit cols-left))]
               [content     (gap-substring gb seg-start seg-end)]
               [display-len (for/sum ([ch (in-string content)])
                              (max 0 (char-display-width ch)))])
          (loop (if (< line-end len) (add1 line-end) len)
                (add1 row)
                (cons (visual-line seg-start content #f trunc? display-len) acc))))))

;; ============================================================
;; wrap-lines — split logical lines at max-cols
;; ============================================================

(define (wrap-lines gb start-pos max-rows max-cols)
  (define len (gap-length gb))
  (define (nl? b) (= b #x0A))
  (let buffer-loop ([buf-pos start-pos] [row 0] [acc '()])
    (if (or (>= row max-rows) (>= buf-pos len))
        (reverse acc)
        (let* ([line-end   (gap-scan-byte gb buf-pos 'forward nl?)]
               [line-limit (if (< line-end len) line-end len)])
          (let visual-loop ([seg-pos buf-pos] [vrow row] [seg-acc acc] [cont? #f])
            (define seg-end     (scan-display-width gb seg-pos line-limit max-cols))
            (define content     (gap-substring gb seg-pos seg-end))
            (define display-len (for/sum ([ch (in-string content)])
                                  (max 0 (char-display-width ch))))
            (define vl (visual-line seg-pos content cont? #f display-len))
            (cond
              [(>= seg-end line-limit)
               (buffer-loop (if (< line-end len) (add1 line-end) len)
                            (add1 vrow) (cons vl seg-acc))]
              [(>= (add1 vrow) max-rows)
               (reverse (cons vl seg-acc))]
              [else
               (visual-loop seg-end (add1 vrow) (cons vl seg-acc) #t)]))))))

;; ============================================================
;; visual-line-lines — dispatcher
;; ============================================================

(define (visual-line-lines gb start-pos max-rows max-cols
                           #:wrap-mode [wrap 'none]
                           #:left-col  [left 0])
  (if (eq? wrap 'none)
      (truncate-lines gb start-pos max-rows max-cols left)
      (wrap-lines gb start-pos max-rows max-cols)))

;; ============================================================
;; pos->row-col
;; ============================================================

(define (pos->row-col gb start-pos target-pos)
  (define len (gap-length gb))
  (define (nl? b) (= b #x0A))
  (let loop ([row 0] [pos start-pos])
    (cond [(>= pos target-pos) (values row (gap-display-width gb pos target-pos))]
          [(>= pos len)        (values row (gap-display-width gb pos target-pos))]
          [else (define nl (gap-scan-byte gb pos 'forward nl?))
                (cond [(or (>= nl target-pos) (>= nl len))
                       (values row (gap-display-width gb pos target-pos))]
                      [else (loop (add1 row) (add1 nl))])])))

;; ============================================================
;; Per-buffer state
;; ============================================================

(define wrap-mode-table (make-hasheq))
(define hscroll-table  (make-hasheq))

(define (buffer-wrap-mode buf) (hash-ref wrap-mode-table buf 'none))
(define (set-buffer-wrap-mode! buf m) (hash-set! wrap-mode-table buf m))
(define (truncate-lines? [buf (current-buffer)])
  (eq? (buffer-wrap-mode buf) 'none))

(define (buffer-hscroll buf) (hash-ref hscroll-table buf 0))
(define (set-buffer-hscroll! buf n) (hash-set! hscroll-table buf (max 0 n)))

(define current-buffer (make-parameter #f))
(define (set-buffer buf) (current-buffer buf) buf)
