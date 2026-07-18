#lang racket

;; lang/font-lock.rkt — Syntax highlighting engine
;;
;; Scans buffer text using syntax-table and keyword rules, writes
;; face symbols to text-properties.
;;
;; Two passes:
;;   syntax-scan!   — syntax-table-driven (strings, comments, blocks)
;;   keyword-scan!  — regex-driven keyword highlighting
;;
;; This module writes face SYMBOLS (e.g. 'font-lock-keyword-face),
;; not face-ids.  The render layer resolves symbols to face-ids via
;; display/face.rkt.
;;
;; Architecture:
;;   syntax.rkt          — syntax-table (character classification)
;;   font-lock.rkt       — scanning engine (writes face symbols to text-props)
;;   display/render.rkt  — reads text-props, resolves face-id
;;
;; Dependencies: kernel/data (gap, query, textprop), lang/syntax

(require "../kernel/data/gap.rkt"
         "../kernel/data/query.rkt"
         "../kernel/data/textprop.rkt"
         "syntax.rkt")

(provide
 ;; config
 syntax-config? make-syntax-config
 syntax-config-syntax-table
 syntax-config-keywords
 syntax-config-case-fold?

 ;; passes (write to text-props)
 syntax-scan! keyword-scan!

 ;; passes (pure — return face lists, thread-safe)
 syntax-scan/list keyword-scan/list

 ;; orchestration
 syntax-highlight-region!
 syntax-highlight-changed!

 ;; region extension (shared with bracket-cache)
 extend-change-region)

;; ============================================================
;; Config
;; ============================================================

(struct syntax-config
  (syntax-table  ; syntax-table? | #f
   keywords      ; (listof (cons pregexp? symbol?)) — first match wins
   case-fold?)   ; boolean?
  #:transparent)

(define (make-syntax-config
         #:syntax-table [st #f]
         #:keywords    [keywords '()]
         #:case-fold?  [case-fold? #f])
  (syntax-config st keywords case-fold?))

;; ============================================================
;; Syntax pass — syntax-table-driven
;; ============================================================

;; Pure version: returns (listof (list start end face-name)).
;; Can run in any thread — only reads from gb, never writes.
(define (syntax-scan/list gb st beg end)
  (define len (min end (gap-length gb)))
  (define multi-rules (and st (syntax-table-multi-rules st)))
  (define state       'normal)
  (define depth        0)
  (define mark-start   #f)
  (define current-rule #f)
  (define current-delim #f)
  (define faces-rev '())

  (let loop ([pos beg])
    (when (< pos len)
      (define ch   (gap-char gb pos))
      (define pos1 (gap-next-char-pos gb pos))

      (define matched-rule
        (and multi-rules
             (for/or ([r (in-list multi-rules)])
               (and (gap-match-str-at gb pos (multi-char-rule-start-str r)) r))))

      (case state
        [(normal)
         (cond
           [matched-rule
            (set! current-rule matched-rule)
            (set! mark-start pos)
            (set! depth 1)
            (set! state (multi-char-rule-tag matched-rule))
            (define start-len (string-length (multi-char-rule-start-str matched-rule)))
            (define after-start (gap-skip-n gb pos start-len))
            (if (multi-char-rule-delim-capture? matched-rule)
                (let*-values ([(delim delim-end) (gap-read-delim-word gb after-start)]
                              [(nl) (gap-scan-byte gb delim-end 'forward
                                                   (λ (b) (= b #x0A)))])
                  (set! current-delim delim)
                  (if (< nl len) (loop (add1 nl)) (loop len)))
                (loop after-start))]
           [(and st (char-string-quote? ch st))
            (set! mark-start pos) (set! state 'string) (loop pos1)]
           [(and st (char-comment-start? ch st))
            (define nl (gap-scan-byte gb pos 'forward (λ (b) (= b #x0A))))
            (define ce (min nl len))
            (set! faces-rev (cons (list pos ce 'font-lock-comment-face) faces-rev))
            (if (< nl len) (loop (add1 nl)) (loop len))]
           [else (loop pos1)])]

        [(string)
         (cond
           [(and st (char-escape? ch st))
            (if (< pos1 len) (loop (gap-skip-n gb pos 2)) (loop len))]
           [(and st (char-string-quote? ch st))
            (set! faces-rev (cons (list mark-start pos1 'font-lock-string-face) faces-rev))
            (set! state 'normal) (loop pos1)]
           [else (loop pos1)])]

        [else
         (define end-str   (multi-char-rule-end-str current-rule))
         (define start-str (multi-char-rule-start-str current-rule))
         (cond
           [(multi-char-rule-delim-capture? current-rule)
            (cond [(and (gap-at-bol? gb pos) (gap-match-str-at gb pos current-delim))
                   (define delim-end (gap-skip-n gb pos (string-length current-delim)))
                   (cond [(>= delim-end len)
                          (set! faces-rev (cons (list mark-start delim-end 'font-lock-string-face) faces-rev))
                          (set! state 'normal) (loop delim-end)]
                         [(char=? (gap-char gb delim-end) #\newline)
                          (set! faces-rev (cons (list mark-start (add1 delim-end) 'font-lock-string-face) faces-rev))
                          (set! state 'normal) (loop (add1 delim-end))]
                         [else
                          (set! faces-rev (cons (list pos pos1 'font-lock-string-face) faces-rev))
                          (loop pos1)])]
                  [else
                   (set! faces-rev (cons (list pos pos1 'font-lock-string-face) faces-rev))
                   (loop pos1)])]
           [(gap-match-str-at gb pos end-str)
            (set! depth (sub1 depth))
            (define pos2 (gap-skip-n gb pos (string-length end-str)))
            (when (zero? depth)
              (set! faces-rev (cons (list mark-start pos2 'font-lock-comment-face) faces-rev))
              (set! state 'normal))
            (loop pos2)]
           [(and (multi-char-rule-nestable? current-rule)
                 (gap-match-str-at gb pos start-str))
            (set! depth (add1 depth))
            (loop (gap-skip-n gb pos (string-length start-str)))]
           [else (loop pos1)])])))
  (reverse faces-rev))

;; Side-effecting wrapper: call /list then write to text-props.
(define (syntax-scan! gb tp st beg end)
  (for ([f (in-list (syntax-scan/list gb st beg end))])
    (match-define (list s e face-name) f)
    (textprop-put! tp s e 'face face-name)))

;; ============================================================
;; Keyword pass — regex match → face symbol
;; ============================================================

;; Pure version: returns (listof (list start end face-name)).
;; Thread-safe — only reads from gb.
(define (keyword-scan/list gb keywords beg end case-fold?)
  (when (null? keywords) (void))
  (define text (gap-substring gb beg end))
  (define tlen (string-length text))
  (define len (gap-length gb))
  (define real-end (min end len))
  (define faces-rev '())

  (define byte-offsets
    (let loop ([pos beg] [i 0] [acc '()])
      (if (or (>= pos real-end) (>= i tlen))
          (list->vector (reverse acc))
          (let ([cl (let-values ([(c l) (gap-char+len gb pos)]) l)])
            (loop (+ pos cl) (add1 i) (cons pos acc))))))

  (for ([kw-entry (in-list keywords)])
    (match-define (cons rx face-name) kw-entry)
    (define pat (cond [(pregexp? rx) rx]
                      [(regexp? rx) rx]
                      [(string? rx) (pregexp rx)]
                      [else (pregexp (format "~a" rx))]))
    (let sloop ([offset 0])
      (when (< offset tlen)
        (define m (regexp-match-positions pat text offset tlen))
        (when m
          (match-define (cons mb me) (car m))
          (define bb (if (< mb (vector-length byte-offsets))
                         (vector-ref byte-offsets mb)
                         (+ beg mb)))
          (define be (if (< me (vector-length byte-offsets))
                         (vector-ref byte-offsets me)
                         real-end))
          (set! faces-rev (cons (list bb be face-name) faces-rev))
          (sloop (max (add1 offset) me))))))
  (reverse faces-rev))

;; Side-effecting wrapper.
(define (keyword-scan! gb tp keywords beg end case-fold?)
  (for ([f (in-list (keyword-scan/list gb keywords beg end case-fold?))])
    (match-define (list s e face-name) f)
    (unless (textprop-get tp s 'face #f)
      (textprop-put! tp s e 'face face-name))))

;; ============================================================
;; Orchestration
;; ============================================================

(define (syntax-highlight-region! gb tp config beg end)
  ;; Clear old faces + run passes for the given region.
  (when (and config (< beg end))
    (textprop-remove-key! tp beg end 'face)
    (when (syntax-config-syntax-table config)
      (syntax-scan! gb tp (syntax-config-syntax-table config) beg end))
    (when (pair? (syntax-config-keywords config))
      (keyword-scan! gb tp
                     (syntax-config-keywords config)
                     beg end
                     (syntax-config-case-fold? config)))))

(define (extend-change-region gb start end #:line-count [lines 15])
  ;; Extend a change extent backward/forward by `lines` to catch
  ;; multi-line constructs (line comments, bracket nesting, etc.).
  ;; Returns (cons sol eol).
  (define buflen (gap-length gb))
  (define sol
    (let loop ([pos start] [remaining lines])
      (if (or (zero? pos) (zero? remaining))
          pos
          (let ([prev-nl (gap-scan-byte gb (sub1 pos) 'backward
                                        (λ (b) (= b #x0A)))])
            (if (>= prev-nl 0)
                (loop (add1 prev-nl) (sub1 remaining))
                0)))))
  (define eol
    (let loop ([pos end] [remaining lines])
      (define nl (gap-scan-byte gb pos 'forward (λ (b) (= b #x0A))))
      (if (or (>= nl buflen) (zero? remaining))
          (min nl buflen)
          (loop (add1 nl) (sub1 remaining)))))
  (cons sol eol))

(define (syntax-highlight-changed! gb tp config change-extent)
  (match-define (cons start end) change-extent)
  (match-define (cons sol eol) (extend-change-region gb start end))
  (syntax-highlight-region! gb tp config sol eol))
