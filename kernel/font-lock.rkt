#lang racket

;; core/font-lock.rkt — Buffer-level fontification engine
;;
;; Pure buffer operations: scans text, writes 'face text properties.
;; Face names are symbols (protocol), visual attributes are in display/face.rkt.
;; Language-specific keyword lists are in modes/ (e.g. modes/racket-keywords.rkt).

(require "buffer.rkt"
         "gap.rkt"
         "textprop.rkt")

(provide
 ;; face name symbols (protocol)
 font-lock-string-face font-lock-comment-face
 font-lock-keyword-face font-lock-builtin-face
 font-lock-constant-face font-lock-function-name-face
 font-lock-type-face font-lock-variable-name-face

 ;; buffer-local config
 font-lock-defaults set-font-lock-defaults!
 font-lock-keywords font-lock-syntax? font-lock-case-fold?

 ;; engine
 font-lock-fontify-buffer font-lock-fontify-region
 font-lock-unfontify-region
 font-lock-after-change-handler)

;; ============================================================
;; Face names (protocol symbols, no colors here)
;; ============================================================

(define font-lock-string-face      'font-lock-string-face)
(define font-lock-comment-face     'font-lock-comment-face)
(define font-lock-keyword-face     'font-lock-keyword-face)
(define font-lock-builtin-face     'font-lock-builtin-face)
(define font-lock-constant-face    'font-lock-constant-face)
(define font-lock-function-name-face 'font-lock-function-name-face)
(define font-lock-type-face        'font-lock-type-face)
(define font-lock-variable-name-face 'font-lock-variable-name-face)

;; ============================================================
;; Per-buffer config
;; ============================================================

(define (font-lock-defaults [buf (current-buffer)])
  (buffer-local buf 'font-lock-defaults '(() #t #f)))
(define (set-font-lock-defaults! kw [syntax? #t] [case-fold? #f] [buf (current-buffer)])
  (set-buffer-local! buf 'font-lock-defaults (list kw syntax? case-fold?)))
(define (font-lock-keywords [buf (current-buffer)]) (first (font-lock-defaults buf)))
(define (font-lock-syntax? [buf (current-buffer)]) (second (font-lock-defaults buf)))
(define (font-lock-case-fold? [buf (current-buffer)]) (third (font-lock-defaults buf)))

;; ============================================================
;; Helpers
;; ============================================================

(define (char-at gb pos) (let-values ([(ch l) (gap-char-at gb pos)]) ch))
(define (char-len gb pos) (let-values ([(ch l) (gap-char-at gb pos)]) l))
(define (skip-n gb pos n) (let loop ([p pos] [i n]) (if (zero? i) p (loop (+ p (char-len gb p)) (sub1 i)))))

;; ============================================================
;; Syntactic pass — strings, comments, block-comments
;; ============================================================

(define (font-lock-syntactic-pass buf beg end)
  (define gb (buffer-gap buf))
  (define len (min end (gap-byte-length gb)))
  (define state 'normal) (define depth 0) (define mark-start #f)
  (let loop ([pos beg])
    (when (< pos len)
      (define ch (char-at gb pos))
      (define pos1 (+ pos (char-len gb pos)))
      (case state
        [(normal)
         (cond [(char=? ch #\#)
                (if (and (< pos1 len) (char=? (char-at gb pos1) #\|))
                    (begin (set! mark-start pos) (set! depth 1)
                           (set! state 'block-comment) (loop (skip-n gb pos 2)))
                    (loop pos1))]
               [(char=? ch #\") (set! mark-start pos) (set! state 'string) (loop pos1)]
               [(char=? ch #\;)
                (define nl (gap-scan-forward-byte gb pos (curry = #x0A)))
                (define ce (min nl len))
                (put-text-property buf pos ce 'face font-lock-comment-face)
                (if (< nl len) (loop (add1 nl)) (loop len))]
               [else (loop pos1)])]
        [(string)
         (cond [(char=? ch #\\) (if (< pos1 len) (loop (skip-n gb pos 2)) (loop len))]
               [(char=? ch #\") (put-text-property buf mark-start pos1 'face font-lock-string-face)
                (set! state 'normal) (loop pos1)]
               [else (loop pos1)])]
        [(block-comment)
         (cond [(and (char=? ch #\|) (< pos1 len) (char=? (char-at gb pos1) #\#))
                (set! depth (sub1 depth))
                (define pos2 (skip-n gb pos 2))
                (when (zero? depth) (put-text-property buf mark-start pos2 'face font-lock-comment-face)
                      (set! state 'normal))
                (loop pos2)]
               [(and (char=? ch #\#) (< pos1 len) (char=? (char-at gb pos1) #\|))
                (set! depth (add1 depth)) (loop (skip-n gb pos 2))]
               [else (loop pos1)])]))))

;; ============================================================
;; Keyword pass — regex match → text property
;; ============================================================

(define (font-lock-keyword-pass buf beg end)
  (define keywords (font-lock-keywords buf))
  (unless (null? keywords)
    (define gb (buffer-gap buf))
    (define text (gap-substring gb beg end))
    (define tlen (string-length text))
    (define byte-offsets
      (let loop ([pos beg] [i 0] [acc '()])
        (if (or (>= pos end) (>= i tlen))
            (list->vector (reverse acc))
            (let ([cl (char-len gb pos)])
              (loop (+ pos cl) (add1 i) (cons pos acc))))))
    (for ([kw (in-list keywords)])
      (match-define (cons rx face-name) kw)
      (define pat (if (string? rx) (pregexp rx) rx))
      (let sloop ([offset 0])
        (when (< offset tlen)
          (define m (regexp-match-positions pat text offset tlen))
          (when m
            (match-define (cons mb me) (car m))
            (define bb (if (< mb tlen) (vector-ref byte-offsets mb) (+ beg mb)))
            (define be (if (< me tlen) (vector-ref byte-offsets me) end))
            (unless (get-text-property buf bb 'face #f)
              (put-text-property buf bb be 'face face-name))
            (sloop (max (add1 offset) me))))))))

;; ============================================================
;; Public API
;; ============================================================

(define (font-lock-unfontify-region buf beg end)
  (when (< beg end)
    (remove-text-properties buf beg end '(face))))

(define (font-lock-fontify-region buf beg end)
  (when (< beg end)
    (font-lock-unfontify-region buf beg end)
    (when (font-lock-syntax? buf)
      (font-lock-syntactic-pass buf beg end))
    (font-lock-keyword-pass buf beg end)))

(define (font-lock-fontify-buffer buf)
  (font-lock-fontify-region buf 0 (buffer-byte-length buf)))

(define font-lock-after-change-handler
  (λ (buf start lendel lenins)
    (define changed-end (+ start (max lendel lenins)))
    (define gb (buffer-gap buf))
    (define sol (let ([nl (gap-scan-backward-byte gb start (curry = #x0A))])
                  (if (>= nl 0) (add1 nl) 0)))
    (define eol (let ([nl (gap-scan-forward-byte gb changed-end (curry = #x0A))])
                  (if (< nl (gap-byte-length gb)) nl (gap-byte-length gb))))
    (font-lock-fontify-region buf sol eol)))
