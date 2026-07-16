#lang racket

;; base/font-lock.rkt — Fontification engine
;;
;; Two passes:
;;   fontify-syntax!    — syntax-table-driven (strings, comments, blocks)
;;   fontify-keywords!  — regex-driven keyword highlighting
;;
;; Composed by fontify-region!, called by fontify-after-change!
;; which reads buffer-change-region for incremental updates.
;;
;; No hooks.  The event loop calls fontify-after-change! explicitly
;; after each modifying command.

(require "../kernel/buffer.rkt"
         "../kernel/text.rkt"
         "../kernel/gap/gap.rkt"
         "../kernel/gap/query.rkt"
         "../kernel/textprop.rkt"
         "../kernel/syntax.rkt"
         "../display/face.rkt")

(provide
 ;; config
 font-lock-config? make-font-lock-config
 font-lock-config-keywords
 font-lock-config-syntax?
 font-lock-config-case-fold?
 set-buffer-font-lock-config!
 buffer-font-lock-config

 ;; passes (exported for composition)
 fontify-syntax!
 fontify-keywords!
 buffer-fontify-passes

 ;; orchestration
 fontify-region!
 fontify-after-change!)

;; ============================================================
;; Config
;; ============================================================

(struct font-lock-config
  (keywords      ; (listof (cons pregexp? symbol?)) — match order = priority
   syntax?       ; boolean? — enable syntax pass?
   case-fold?)   ; boolean? — case-insensitive keyword match?
  #:transparent)

(define (make-font-lock-config
         #:keywords [keywords '()]
         #:syntax?  [syntax? #t]
         #:case-fold? [case-fold? #f])
  (font-lock-config keywords syntax? case-fold?))

;; Per-buffer storage
(define font-lock-config-table (make-hasheq))
(define fontify-passes-table  (make-hasheq))

(define (set-buffer-font-lock-config! buf config)
  (hash-set! font-lock-config-table buf config))

(define (buffer-font-lock-config buf)
  (hash-ref font-lock-config-table buf (λ () #f)))

;; ============================================================
;; Buffer-side syntax table storage
;; ============================================================

(define syntax-table-storage (make-hasheq))

(define (buffer-syntax-table buf)
  (hash-ref syntax-table-storage buf (λ () #f)))

(define (set-buffer-syntax-table! buf st)
  (hash-set! syntax-table-storage buf st))

(provide buffer-syntax-table set-buffer-syntax-table!)

;; ============================================================
;; Syntax pass — data-driven: reads syntax-table rules
;; ============================================================

(define (fontify-syntax! buf beg end)
  (define st (buffer-syntax-table buf))
  (unless st (void))
  (define gb (text-gap (buffer-text buf)))
  (define len (min end (gap-length gb)))
  (define tp (buffer-text-props buf))
  (define multi-rules (syntax-table-multi-rules st))

  (define state 'normal)
  (define depth 0)
  (define mark-start #f)
  (define current-rule #f)
  (define current-delim #f)

  (let loop ([pos beg])
    (when (< pos len)
      (define ch (gap-char gb pos))
      (define pos1 (gap-next-char-pos gb pos))

      ;; Match any multi-char rule start at this position
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
            (define start-len (string-length (multi-char-rule-start-str matched-rule)))
            (define after-start (gap-skip-n gb pos start-len))
            (if (multi-char-rule-delim-capture? matched-rule)
                ;; Heredoc: #<<DELIM — capture delimiter, skip to next line
                (let*-values ([(delim delim-end) (gap-read-delim-word gb after-start)]
                              [(nl) (gap-scan-byte gb delim-end 'forward (curry = #x0A))])
                  (set! current-delim delim)
                  (if (< nl len) (loop (add1 nl)) (loop len)))
                (loop after-start))]
           [(char-string-quote? ch st)
            (set! mark-start pos) (set! state 'string) (loop pos1)]
           [(char-comment-start? ch st)
            ;; Line comment: face from here to end of line
            (define nl (gap-scan-byte gb pos 'forward (curry = #x0A)))
            (define ce (min nl len))
            (textprop-put! tp pos ce 'face 'font-lock-comment-face)
            (if (< nl len) (loop (add1 nl)) (loop len))]
           [else (loop pos1)])]

        ;; ── string ──
        [(string)
         (cond
           [(char-escape? ch st)
            (if (< pos1 len) (loop (gap-skip-n gb pos 2)) (loop len))]
           [(char-string-quote? ch st)
            (textprop-put! tp mark-start pos1 'face 'font-lock-string-face)
            (set! state 'normal) (loop pos1)]
           [else (loop pos1)])]

        ;; ── multi-char rule state ──
        [else
         (define end-str (multi-char-rule-end-str current-rule))
         (define start-str (multi-char-rule-start-str current-rule))
         (cond
           [(multi-char-rule-delim-capture? current-rule)
            ;; Heredoc: end when current-delim at beginning of line
            (cond [(and (gap-at-bol? gb pos)
                        (gap-match-str-at gb pos current-delim))
                   (define delim-end (gap-skip-n gb pos (string-length current-delim)))
                   (cond [(>= delim-end len)
                          (textprop-put! tp mark-start delim-end 'face 'font-lock-string-face)
                          (set! state 'normal) (loop delim-end)]
                         [(char=? (gap-char gb delim-end) #\newline)
                          (textprop-put! tp mark-start (add1 delim-end) 'face 'font-lock-string-face)
                          (set! state 'normal) (loop (add1 delim-end))]
                         [else
                          (textprop-put! tp pos pos1 'face 'font-lock-string-face)
                          (loop pos1)])]
                  [else
                   (textprop-put! tp pos pos1 'face 'font-lock-string-face)
                   (loop pos1)])]
           [(gap-match-str-at gb pos end-str)
            (set! depth (sub1 depth))
            (define pos2 (gap-skip-n gb pos (string-length end-str)))
            (when (zero? depth)
              (define face-name
                (case state
                  [(block-comment) 'font-lock-comment-face]
                  [else 'font-lock-comment-face]))
              (textprop-put! tp mark-start pos2 'face face-name)
              (set! state 'normal))
            (loop pos2)]
           [(and (multi-char-rule-nestable? current-rule)
                 (gap-match-str-at gb pos start-str))
            (set! depth (add1 depth))
            (loop (gap-skip-n gb pos (string-length start-str)))]
           [else (loop pos1)])]))))

;; ============================================================
;; Keyword pass — regex match → write 'face text property
;; ============================================================

(define (fontify-keywords! buf beg end)
  (define config (buffer-font-lock-config buf))
  (unless config (void))
  (define keywords (font-lock-config-keywords config))
  (when (null? keywords) (void))

  (define gb (text-gap (buffer-text buf)))
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

  (define tp (buffer-text-props buf))
  (define case-fold? (font-lock-config-case-fold? config))

  ;; Each keyword entry: (pregexp . face-name)
  ;; First match wins — earlier entries have higher priority.
  (for ([kw-entry (in-list keywords)])
    (match-define (cons rx face-name) kw-entry)
    (define pat (if (pregexp? rx) rx (pregexp rx)))
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
          ;; Only write if no face already set at this position
          (unless (textprop-get tp bb 'face #f)
            (textprop-put! tp bb be 'face face-name))
          (sloop (max (add1 offset) me)))))))

;; ============================================================
;; Pass composition
;; ============================================================

(define (buffer-fontify-passes buf)
  (hash-ref fontify-passes-table buf
    (λ ()
      ;; Default passes — each checks its own activation
      (list
       (λ (b beg end)
         (when (and (buffer-syntax-table b)
                    (let ([cfg (buffer-font-lock-config b)])
                      (or (not cfg) (font-lock-config-syntax? cfg))))
           (fontify-syntax! b beg end)))
       (λ (b beg end)
         (when (and (buffer-font-lock-config b)
                    (not (null? (font-lock-config-keywords
                                 (buffer-font-lock-config b)))))
           (fontify-keywords! b beg end)))))))

;; ============================================================
;; Orchestration
;; ============================================================

(define (fontify-region! buf beg end)
  (when (< beg end)
    (define tp (buffer-text-props buf))
    ;; Clear old faces in this region
    (textprop-remove! tp beg end)
    ;; Run all active passes
    (for ([pass (in-list (buffer-fontify-passes buf))])
      (pass buf beg end))))

(define (fontify-after-change! buf)
  ;; Read the changed region, extend backward/forward to catch
  ;; multi-line constructs (block comments, heredoc, multi-line strings).
  (define changed (buffer-change-region buf))
  (unless changed (void))
  (match-define (cons start end) changed)
  (define gb (text-gap (buffer-text buf)))
  (define buflen (gap-length gb))

  ;; Extend backward by up to 15 lines
  (define sol
    (let loop ([pos start] [remaining 15])
      (if (or (zero? pos) (zero? remaining))
          pos
          (let ([prev-nl (gap-scan-byte gb (sub1 pos) 'backward (curry = #x0A))])
            (if (>= prev-nl 0) (loop (add1 prev-nl) (sub1 remaining)) pos)))))

  ;; Extend forward by up to 15 lines
  (define eol
    (let loop ([pos end] [remaining 15])
      (define nl (gap-scan-byte gb pos 'forward (curry = #x0A)))
      (if (or (>= nl buflen) (zero? remaining))
          (min nl buflen)
          (loop (add1 nl) (sub1 remaining)))))

  (fontify-region! buf sol eol))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit
           "../display/face.rkt")

  (init-face-cache!)
  (define-face! 'font-lock-comment-face
    (make-face-attrs 'foreground (list 100 160 100) 'slant 'italic))
  (define-face! 'font-lock-string-face
    (make-face-attrs 'foreground (list 80 180 80)))
  (define-face! 'font-lock-keyword-face
    (make-face-attrs 'foreground (list 50 150 255) 'weight 'bold))

  ;; Test: line comment
  (let ([buf (make-buffer "test" ";; comment\n(define x 42)")])
    (define st (make-racket-syntax-table))
    (set-buffer-syntax-table! buf st)
    (fontify-syntax! buf 0 (buffer-length buf))
    (check-equal? (buffer-face-at buf 2) 'font-lock-comment-face)
    (check-equal? (buffer-face-at buf 13) #f))

  ;; Test: string
  (let ([buf (make-buffer "test" "\"hello world\"")])
    (define st (make-racket-syntax-table))
    (set-buffer-syntax-table! buf st)
    (fontify-syntax! buf 0 (buffer-length buf))
    (check-equal? (buffer-face-at buf 2) 'font-lock-string-face)
    (check-equal? (buffer-face-at buf 0) 'font-lock-string-face))

  ;; Test: block comment
  (let ([buf (make-buffer "test" "#| block |# after")])
    (define st (make-racket-syntax-table))
    (set-buffer-syntax-table! buf st)
    (fontify-syntax! buf 0 (buffer-length buf))
    (check-equal? (buffer-face-at buf 3) 'font-lock-comment-face))

  ;; Test: keyword
  (let ([buf (make-buffer "test" "(define foo 42)")])
    (define config
      (make-font-lock-config
       #:keywords (list (cons (pregexp "\\bdefine\\b") 'font-lock-keyword-face))))
    (set-buffer-font-lock-config! buf config)
    (fontify-keywords! buf 0 (buffer-length buf))
    (check-equal? (buffer-face-at buf 2) 'font-lock-keyword-face))
)
