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

 ;; passes
 syntax-scan!
 keyword-scan!

 ;; orchestration
 syntax-highlight-region!
 syntax-highlight-changed!)

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

(define (syntax-scan! gb tp st beg end)
  ;; Walk bytes [beg, end), assign face symbols based on syntax-table st.
  ;; Handles: line comments, strings, block-comments, heredoc.
  (define len (min end (gap-length gb)))
  (define multi-rules (and st (syntax-table-multi-rules st)))

  (define state       'normal)
  (define depth        0)
  (define mark-start   #f)
  (define current-rule #f)
  (define current-delim #f)

  (let loop ([pos beg])
    (when (< pos len)
      (define ch   (gap-char gb pos))
      (define pos1 (gap-next-char-pos gb pos))

      (define matched-rule
        (and multi-rules
             (for/or ([r (in-list multi-rules)])
               (and (gap-match-str-at gb pos (multi-char-rule-start-str r)) r))))

      (case state
        ;; ── normal ──
        [(normal)
         (cond
           [matched-rule
            (set! current-rule matched-rule)
            (set! mark-start pos)
            (set! depth 1)
            (set! state (multi-char-rule-tag matched-rule))
            (define start-len
              (string-length (multi-char-rule-start-str matched-rule)))
            (define after-start (gap-skip-n gb pos start-len))
            (if (multi-char-rule-delim-capture? matched-rule)
                (let*-values ([(delim delim-end)
                               (gap-read-delim-word gb after-start)]
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
            (textprop-put! tp pos ce 'face 'font-lock-comment-face)
            (if (< nl len) (loop (add1 nl)) (loop len))]
           [else (loop pos1)])]

        ;; ── string ──
        [(string)
         (cond
           [(and st (char-escape? ch st))
            (if (< pos1 len) (loop (gap-skip-n gb pos 2)) (loop len))]
           [(and st (char-string-quote? ch st))
            (textprop-put! tp mark-start pos1 'face 'font-lock-string-face)
            (set! state 'normal) (loop pos1)]
           [else (loop pos1)])]

        ;; ── multi-char (block-comment, heredoc) ──
        [else
         (define end-str   (multi-char-rule-end-str current-rule))
         (define start-str (multi-char-rule-start-str current-rule))
         (cond
           [(multi-char-rule-delim-capture? current-rule)
            (cond [(and (gap-at-bol? gb pos)
                        (gap-match-str-at gb pos current-delim))
                   (define delim-end
                     (gap-skip-n gb pos (string-length current-delim)))
                   (cond [(>= delim-end len)
                          (textprop-put! tp mark-start delim-end
                                         'face 'font-lock-string-face)
                          (set! state 'normal) (loop delim-end)]
                         [(char=? (gap-char gb delim-end) #\newline)
                          (textprop-put! tp mark-start (add1 delim-end)
                                         'face 'font-lock-string-face)
                          (set! state 'normal) (loop (add1 delim-end))]
                         [else
                          (textprop-put! tp pos pos1
                                         'face 'font-lock-string-face)
                          (loop pos1)])]
                  [else
                   (textprop-put! tp pos pos1 'face 'font-lock-string-face)
                   (loop pos1)])]
           [(gap-match-str-at gb pos end-str)
            (set! depth (sub1 depth))
            (define pos2 (gap-skip-n gb pos (string-length end-str)))
            (when (zero? depth)
              (textprop-put! tp mark-start pos2
                             'face 'font-lock-comment-face)
              (set! state 'normal))
            (loop pos2)]
           [(and (multi-char-rule-nestable? current-rule)
                 (gap-match-str-at gb pos start-str))
            (set! depth (add1 depth))
            (loop (gap-skip-n gb pos (string-length start-str)))]
           [else (loop pos1)])]))))

;; ============================================================
;; Keyword pass — regex match → face symbol
;; ============================================================

(define (keyword-scan! gb tp keywords beg end case-fold?)
  ;; Match regex keywords in [beg, end).  Only write face if position
  ;; doesn't already have a face (syntax pass has priority).
  (when (null? keywords) (void))

  (define text (gap-substring gb beg end))
  (define tlen (string-length text))
  (define len (gap-length gb))
  (define real-end (min end len))

  ;; Build byte-offset map: char-index → byte-pos
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
          (unless (textprop-get tp bb 'face #f)
            (textprop-put! tp bb be 'face face-name))
          (sloop (max (add1 offset) me)))))))

;; ============================================================
;; Orchestration
;; ============================================================

(define (syntax-highlight-region! gb tp config beg end)
  ;; Clear old faces + run passes for the given region.
  (when (and config (< beg end))
    (textprop-remove! tp beg end)
    (when (syntax-config-syntax-table config)
      (syntax-scan! gb tp (syntax-config-syntax-table config) beg end))
    (when (pair? (syntax-config-keywords config))
      (keyword-scan! gb tp
                     (syntax-config-keywords config)
                     beg end
                     (syntax-config-case-fold? config)))))

(define (syntax-highlight-changed! gb tp config change-extent)
  ;; Incremental: extend change region to catch multi-line constructs,
  ;; then highlight.  Line comments need the comment-start char (;) to
  ;; be within the scan range; when no newline exists between the edit
  ;; point and the comment marker (same line), we fall back to buffer start.
  (match-define (cons start end) change-extent)
  (define buflen (gap-length gb))

  ;; Extend backward up to 15 lines.  If no newline found (first line
  ;; of buffer), go to position 0 to catch comment-start chars.
  (define sol
    (let loop ([pos start] [remaining 15])
      (if (or (zero? pos) (zero? remaining))
          pos
          (let ([prev-nl (gap-scan-byte gb (sub1 pos) 'backward
                                        (λ (b) (= b #x0A)))])
            (if (>= prev-nl 0)
                (loop (add1 prev-nl) (sub1 remaining))
                ;; No newline before edit point — scan from buffer start
                0)))))

  ;; Extend forward up to 15 lines
  (define eol
    (let loop ([pos end] [remaining 15])
      (define nl (gap-scan-byte gb pos 'forward (λ (b) (= b #x0A))))
      (if (or (>= nl buflen) (zero? remaining))
          (min nl buflen)
          (loop (add1 nl) (sub1 remaining)))))

  (syntax-highlight-region! gb tp config sol eol))
