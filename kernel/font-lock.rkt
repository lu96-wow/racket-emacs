#lang racket

;; core/font-lock.rkt — Buffer-level fontification engine
;;
;; Pure buffer operations: scans text, writes 'face text properties.
;; Face names are symbols (protocol), visual attributes are in display/face.rkt.
;; Language-specific keyword lists are in modes/ (e.g. modes/racket-keywords.rkt).

(require "buffer.rkt"
         "gap.rkt"
         "textprop.rkt"
         "syntax.rkt")

(provide
 ;; face name symbols (protocol)
 font-lock-string-face font-lock-comment-face
 font-lock-keyword-face font-lock-builtin-face
 font-lock-constant-face font-lock-function-name-face
 font-lock-type-face font-lock-variable-name-face
 ;; paren-depth faces (protocol)
 font-lock-paren-face-1 font-lock-paren-face-2 font-lock-paren-face-3
 font-lock-paren-face-4 font-lock-paren-face-5 font-lock-paren-face-6
 font-lock-paren-face-7 font-lock-paren-face-8 font-lock-paren-face-9
 font-lock-paren-face-10 font-lock-paren-face-11 font-lock-paren-face-12
 paren-depth-faces

 ;; buffer-var config
 font-lock-defaults set-font-lock-defaults!
 font-lock-keywords font-lock-syntax? font-lock-case-fold?

 ;; composable fontify passes
 buffer-fontify-passes set-buffer-fontify-passes!
 default-fontify-passes
 fontify-syntax-pass fontify-keywords-pass fontify-paren-depth-pass

 ;; engine
 fontify-buffer! fontify-region!
 unfontify-region!
 fontify-after-change!)

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

(define font-lock-paren-face-1 'font-lock-paren-face-1)
(define font-lock-paren-face-2 'font-lock-paren-face-2)
(define font-lock-paren-face-3 'font-lock-paren-face-3)
(define font-lock-paren-face-4 'font-lock-paren-face-4)
(define font-lock-paren-face-5 'font-lock-paren-face-5)
(define font-lock-paren-face-6 'font-lock-paren-face-6)
(define font-lock-paren-face-7 'font-lock-paren-face-7)
(define font-lock-paren-face-8 'font-lock-paren-face-8)
(define font-lock-paren-face-9 'font-lock-paren-face-9)
(define font-lock-paren-face-10 'font-lock-paren-face-10)
(define font-lock-paren-face-11 'font-lock-paren-face-11)
(define font-lock-paren-face-12 'font-lock-paren-face-12)

(define paren-depth-faces
  (vector font-lock-paren-face-1
          font-lock-paren-face-2
          font-lock-paren-face-3
          font-lock-paren-face-4
          font-lock-paren-face-5
          font-lock-paren-face-6
          font-lock-paren-face-7
          font-lock-paren-face-8
          font-lock-paren-face-9
          font-lock-paren-face-10
          font-lock-paren-face-11
          font-lock-paren-face-12))

;; ============================================================
;; Per-buffer config
;; ============================================================

(define (font-lock-defaults [buf (current-buffer)])
  (buffer-var buf 'font-lock-defaults '(() #t #f)))
(define (set-font-lock-defaults! kw [syntax? #t] [case-fold? #f] [buf (current-buffer)])
  (set-buffer-var! buf 'font-lock-defaults (list kw syntax? case-fold?)))
(define (font-lock-keywords [buf (current-buffer)]) (first (font-lock-defaults buf)))
(define (font-lock-syntax? [buf (current-buffer)]) (second (font-lock-defaults buf)))
(define (font-lock-case-fold? [buf (current-buffer)]) (third (font-lock-defaults buf)))

;; ============================================================
;; Composable fontify passes — each is (buf beg end) -> void
;; ============================================================

(define fontify-passes-table (make-hasheq))  ; buffer → (listof (buf beg end -> void))

(define (buffer-fontify-passes buf)
  (hash-ref fontify-passes-table buf
    (λ () default-fontify-passes)))

(define (set-buffer-fontify-passes! buf passes)
  (hash-set! fontify-passes-table buf passes))

;; Guarded wrappers: each pass checks its own activation condition.
(define (fontify-syntax-pass buf beg end)
  (when (font-lock-syntax? buf)
    (fontify-syntax! buf beg end)))

(define (fontify-keywords-pass buf beg end)
  ;; fontify-keywords! already guards on empty keyword list internally
  (fontify-keywords! buf beg end))

(define (fontify-paren-depth-pass buf beg end)
  ;; fontify-paren-depth! already guards on missing syntax-table internally
  (fontify-paren-depth! buf beg end))

(define default-fontify-passes
  (list fontify-syntax-pass
        fontify-keywords-pass))

;; ============================================================
;; Helpers
;; ============================================================

(define (char-at gb pos) (let-values ([(ch l) (gap-char-at gb pos)]) ch))
(define (char-len gb pos) (let-values ([(ch l) (gap-char-at gb pos)]) l))
(define (skip-n gb pos n) (let loop ([p pos] [i n]) (if (zero? i) p (loop (+ p (char-len gb p)) (sub1 i)))))

;; ============================================================
;; Helpers — gap-aware string prefix match
;; ============================================================

;; Does string s match at byte position pos in gap-buffer gb (up to len)?
(define (match-str-at gb pos len s)
  (define slen (string-length s))
  (and (<= (+ pos slen) len)  ; avoid overshooting if last bytes are partial
       (let loop ([i 0] [p pos])
         (if (= i slen)
             #t
             (let-values ([(ch cl) (gap-char-at gb p)])
               (and (char=? ch (string-ref s i))
                    (loop (add1 i) (+ p cl))))))))

;; ── delim-capture helpers (for #<<DELIM ... DELIM style) ──

;; Read a non-whitespace word starting at pos; returns (values word-str end-pos).
(define (read-delim-word gb pos len)
  (let loop ([p pos] [chars '()])
    (if (>= p len)
        (values (list->string (reverse chars)) p)
        (let-values ([(ch cl) (gap-char-at gb p)])
          (if (or (char=? ch #\space) (char=? ch #\tab)
                  (char=? ch #\newline) (char=? ch #\return))
              (values (list->string (reverse chars)) p)
              (loop (+ p cl) (cons ch chars)))))))

;; Is pos at the beginning of a line?
(define (at-bol? gb pos)
  (or (zero? pos)
      (let ([prev (gap-prev-char-pos gb pos)])
        (and prev (char=? (char-at gb prev) #\newline)))))

;; ============================================================
;; Syntactic pass — data-driven: reads rules from syntax-table
;; ============================================================

(define (fontify-syntax! buf beg end)
  (define gb (buffer-gap buf))
  (define len (min end (gap-byte-length gb)))
  (define st (buffer-syntax-table buf))
  (define multi-rules (and st (syntax-table-multi-rules st)))
  (define state 'normal)
  (define depth 0)
  (define mark-start #f)
  (define current-rule #f)
  (define current-delim #f)

  (let loop ([pos beg])
    (when (< pos len)
      (define ch (char-at gb pos))
      (define pos1 (+ pos (char-len gb pos)))

      ;; ── find the first multi-char rule whose start matches at pos ──
      (define matched-rule
        (and multi-rules
             (for/or ([r (in-list multi-rules)])
               (and (match-str-at gb pos len (multi-char-rule-start r))
                    r))))

      (case state
        ;; ── normal state ──
        [(normal)
         (cond
           [matched-rule
            (set! current-rule matched-rule)
            (set! mark-start pos)
            (set! depth 1)
            (set! state (multi-char-rule-tag matched-rule))
            (define start-len (string-length (multi-char-rule-start matched-rule)))
            (define after-start (skip-n gb pos start-len))
            (if (multi-char-rule-delim-capture? matched-rule)
                ;; heredoc: #<<DELIM — capture delimiter word, skip to next line
                (let*-values ([(delim delim-end) (read-delim-word gb after-start len)]
                              [(nl) (gap-scan-forward-byte gb delim-end (curry = #x0A))])
                  (set! current-delim delim)
                  (if (< nl len) (loop (add1 nl)) (loop len)))
                ;; fixed-delimiter rule
                (loop after-start))]
           [(and st (char-string-quote? ch st))
            (set! mark-start pos) (set! state 'string) (loop pos1)]
           [(and st (char-comment-start? ch st))
            (define nl (gap-scan-forward-byte gb pos (curry = #x0A)))
            (define ce (min nl len))
            (put-text-property buf pos ce 'face font-lock-comment-face)
            (if (< nl len) (loop (add1 nl)) (loop len))]
           [else (loop pos1)])]

        ;; ── string state ──
        [(string)
         (cond
           [(and st (char-escape? ch st))
            (if (< pos1 len) (loop (skip-n gb pos 2)) (loop len))]
           [(and st (char-string-quote? ch st))
            (put-text-property buf mark-start pos1 'face font-lock-string-face)
            (set! state 'normal) (loop pos1)]
           [else (loop pos1)])]

        ;; ── multi-char rule state (state = rule tag, e.g. 'block-comment / 'heredoc) ──
        [else
         (define end-str (multi-char-rule-end current-rule))
         (define start-str (multi-char-rule-start current-rule))
         (cond
           [(multi-char-rule-delim-capture? current-rule)
            ;; heredoc: end when current-delim appears at beginning of line
            (cond
              [(and (at-bol? gb pos)
                    (match-str-at gb pos len current-delim))
               (define delim-end (skip-n gb pos (string-length current-delim)))
               (cond
                 [(>= delim-end len)
                  (put-text-property buf mark-start delim-end 'face font-lock-comment-face)
                  (set! state 'normal) (loop delim-end)]
                 [(char=? (char-at gb delim-end) #\newline)
                  (put-text-property buf mark-start (add1 delim-end) 'face font-lock-comment-face)
                  (set! state 'normal) (loop (add1 delim-end))]
                 [else
                  (put-text-property buf pos pos1 'face font-lock-comment-face)
                  (loop pos1)])]
              [else
               ;; Apply face incrementally — covers partial regions
               ;; when the closing delimiter is outside the fontify range.
               (put-text-property buf pos pos1 'face font-lock-comment-face)
               (loop pos1)])]
           [(match-str-at gb pos len end-str)
            (set! depth (sub1 depth))
            (define pos2 (skip-n gb pos (string-length end-str)))
            (when (zero? depth)
              (put-text-property buf mark-start pos2 'face font-lock-comment-face)
              (set! state 'normal))
            (loop pos2)]
           [(and (multi-char-rule-nestable? current-rule)
                 (match-str-at gb pos len start-str))
            (set! depth (add1 depth))
            (loop (skip-n gb pos (string-length start-str)))]
           [else (loop pos1)])]))))

;; ============================================================
;; Keyword pass — regex match → text property
;; ============================================================

(define (fontify-keywords! buf beg end)
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
;; Paren-depth pass — rainbow brackets, data-driven via syntax-table
;; ============================================================

(define (fontify-paren-depth! buf beg end)
  (define st (buffer-syntax-table buf))
  (when st
    (define gb (buffer-gap buf))
    (define buflen (gap-byte-length gb))
    (define real-end (min end buflen))

    ;; ── Compute starting depth at `beg` by scanning from buffer start ──
    ;;     IMPORTANT: do NOT skip faced brackets here!  When re-fontifying
    ;;     after an edit, positions [0, beg) may still have face properties
    ;;     from the previous pass (e.g. font-lock-paren-face-N on brackets).
    ;;     Those brackets are REAL brackets, not string/comment brackets,
    ;;     and must be counted for correct depth.
    (define start-depth
      (let loop ([pos 0] [d 0])
        (if (>= pos beg)
            d
            (let*-values ([(ch cl) (gap-char-at gb pos)]
                          [(pos1) (+ pos cl)])
              (cond [(char-open? ch st)  (loop pos1 (add1 d))]
                    [(char-close? ch st) (loop pos1 (sub1 (max 0 d)))]
                    [else (loop pos1 d)])))))

    ;; Clear old paren-depth in this region so we can rebuild.
    (clear-paren-depth! buf beg real-end)

    ;; ── Walk [beg, real-end): mark brackets + set paren-depth on content ──
    (define depth-levels (vector-length paren-depth-faces))
    (let loop ([pos beg] [depth start-depth])
      (when (< pos real-end)
        (let*-values ([(ch cl) (gap-char-at gb pos)]
                      [(pos1) (+ pos cl)])
          (cond
            [(char-open? ch st)
             (define face-idx (modulo depth depth-levels))
             (put-text-property buf pos pos1 'face
                                (vector-ref paren-depth-faces face-idx))
             (loop pos1 (add1 depth))]
            [(char-close? ch st)
             (define close-depth (sub1 (max 0 depth)))
             (define face-idx (modulo close-depth depth-levels))
             (put-text-property buf pos pos1 'face
                                (vector-ref paren-depth-faces face-idx))
             (loop pos1 close-depth)]
            [else
             ;; Non-bracket character at depth > 0: record its depth so the
             ;; renderer can tint its background.  Depth index = (d-1) so it
             ;; matches the enclosing opening bracket's face.
             (when (positive? depth)
               (define pd-idx (modulo (sub1 depth) depth-levels))
               (put-paren-depth buf pos pos1 pd-idx))
             (loop pos1 depth)]))))))

;; ============================================================
;; Public API
;; ============================================================

(define (unfontify-region! buf beg end)
  (when (< beg end)
    (remove-text-properties buf beg end '(face))
    (clear-paren-depth! buf beg end)))

(define (fontify-region! buf beg end)
  (when (< beg end)
    (unfontify-region! buf beg end)
    (for ([pass (in-list (buffer-fontify-passes buf))])
      (pass buf beg end))))

(define (fontify-buffer! buf)
  (fontify-region! buf 0 (buffer-byte-length buf)))

(define fontify-after-change!
  (λ (buf start lendel lenins)
    (define changed-end (+ start (max lendel lenins)))
    (define gb (buffer-gap buf))
    (define buflen (gap-byte-length gb))
    (define line-start (let ([nl (gap-scan-backward-byte gb start (curry = #x0A))])
                         (if (>= nl 0) (add1 nl) 0)))
    ;; Extend backward by up to 15 lines so multi-line constructs
    ;; (heredoc, block-comment) are entered in the correct state.
    (define sol
      (let loop ([pos line-start] [remaining 15])
        (if (or (zero? pos) (zero? remaining))
            pos
            (let ([prev-nl (gap-scan-backward-byte gb (sub1 pos) (curry = #x0A))])
              (if (>= prev-nl 0) (loop (add1 prev-nl) (sub1 remaining)) pos)))))
    ;; Extend forward by up to 15 lines to include the closing delimiter
    ;; of multi-line constructs.
    (define eol
      (let loop ([pos changed-end] [remaining 15])
        (define nl (gap-scan-forward-byte gb pos (curry = #x0A)))
        (if (or (>= nl buflen) (zero? remaining))
            (if (< nl buflen) nl buflen)
            (loop (add1 nl) (sub1 remaining)))))
    (fontify-region! buf sol eol)))
