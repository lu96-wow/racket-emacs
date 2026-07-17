#lang racket

;; lang/bracket-cache.rkt — Bracket depth coloring with checkpoint caching
;;
;; Checkpoint-based: stores parse state snapshots at regular byte intervals,
;; so edits only rescan from the last valid checkpoint forward.  Compares
;; new state against stale (shifted) checkpoints for early convergence.
;;
;; Writes face names to text-properties under the 'bracket-face key,
;; independent from font-lock's 'face key.  Render layer falls back from
;; 'face → 'bracket-face.
;;
;; Architecture:
;;   bracket-cache.rkt   — checkpoint store + incremental scanner
;;   motion.rkt          — sexp scanning (used by bracket-find-match)
;;   font-lock.rkt       — syntax-highlight (uses 'face key)
;;   render.rkt          — reads 'face then 'bracket-face
;;
;; Dependencies: gap, query, textprop, syntax, font-lock (extend-change-region)

(require "../kernel/data/gap.rkt"
         "../kernel/data/query.rkt"
         "../kernel/data/textprop.rkt"
         "../kernel/data/syntax.rkt"
         "../kernel/motion.rkt"
         "../display/face.rkt"
         "font-lock.rkt")      ; for extend-change-region

(provide
 ;; structs
 bracket-cache? bracket-cache-checkpoints bracket-cache-buf-len bracket-cache-interval
 make-bracket-cache

 ;; core operations
 bracket-update!              ; cache gb tp st extent → void
 bracket-rescan-all!          ; cache gb tp st → void

 ;; queries (use cached checkpoints as anchors)
 bracket-state-at             ; cache gb st pos → scan-state | #f
 bracket-find-match           ; cache gb st pos → pos | #f

 ;; face registration (call once at startup)
 bracket-register-faces!)

;; ============================================================
;; Scan state
;; ============================================================

(struct scan-state
  (depth        ; nonnegative-integer — current bracket nesting depth
   mode         ; 'normal | 'string | 'block-comment | 'here-string
   quote-ch     ; char? | #f — string quote being matched
   rule          ; multi-char-rule? | #f — active block-comment / heredoc rule
   block-depth  ; integer — nesting within nestable block-comments
   delim        ; string? | #f — heredoc delimiter word
   stack)        ; (listof char?) — unclosed open-bracket chars, innermost at head
  #:transparent)

(define initial-scan-state
  (scan-state 0 'normal #f #f 0 #f '()))

;; ============================================================
;; Checkpoint
;; ============================================================

(struct checkpoint (pos state) #:transparent)

;; ============================================================
;; Bracket cache
;; ============================================================

(struct bracket-cache
  ([checkpoints #:mutable]  ; (listof checkpoint?) — strictly ascending by pos
   [buf-len     #:mutable]  ; integer — gap-length at last scan completion
   interval)                 ; integer — checkpoint spacing (default 1024)
  #:transparent)

(define (make-bracket-cache [interval 1024])
  (bracket-cache '() 0 interval))

;; ============================================================
;; Face names
;; ============================================================

(define bracket-depth-faces
  (vector 'bracket-depth-0-face
          'bracket-depth-1-face
          'bracket-depth-2-face
          'bracket-depth-3-face
          'bracket-depth-4-face
          'bracket-depth-5-face))

(define bracket-mismatch-face 'bracket-mismatch-face)

(define (bracket-face-for-depth depth)
  (vector-ref bracket-depth-faces (modulo depth (vector-length bracket-depth-faces))))

;; ============================================================
;; bracket-register-faces! — called once at startup
;; ============================================================

(define (bracket-register-faces!)
  (define colors
    (list (list 255 180 0)   ; gold
          (list 180 120 255) ; purple
          (list 80  200 255) ; cyan
          (list 255 100 100) ; red
          (list 100 255 100) ; green
          (list 255 200 80))) ; orange
  (for ([(c i) (in-indexed (in-list colors))])
    (define name (vector-ref bracket-depth-faces i))
    (define-face! name (make-face-attrs 'foreground c)))
  (define-face! bracket-mismatch-face
                (make-face-attrs 'foreground (list 255 255 255)
                                 'background (list 180 0 0))))

;; ============================================================
;; Bracket matching helpers
;; ============================================================

(define (matching-close ch)
  (case ch [(#\() #\)] [(#\[) #\]] [(#\{) #\}] [else #\)]))

(define (matching-open ch)
  (case ch [(#\)) #\(] [(#\]) #\[] [(#\}) #\{] [else #\(]))

;; ============================================================
;; bracket-apply-emit! — write bracket-face for single char
;; ============================================================

(define (bracket-apply-emit! tp pos face-name)
  (define pos2 (add1 pos))  ; all standard brackets are single-byte ASCII
  (when (< pos pos2)
    (textprop-put! tp pos pos2 'bracket-face face-name)))

;; ============================================================
;; advance-char — single char state transition
;; ============================================================
;; Returns (values next-state emit-face? override-next-pos?).
;; override-next-pos: #f → use pos1; non-#f → use this position instead
;; (for multi-char skips like block comment delimiters, heredoc, line comments)

(define (advance-char gb p pos1 ch st rules st-obj)
  (match-define (scan-state depth mode quote-ch rule block-depth delim stack) st)
  (define len (gap-length gb))

  (case mode
    ;; ── normal ──
    [(normal)
     (cond
       ;; Multi-char rule start
       [(for/or ([r (in-list rules)])
          (and (gap-match-str-at gb p (multi-char-rule-start-str r)) r))
        => (λ (r)
            (define tag (multi-char-rule-tag r))
            (define slen (string-length (multi-char-rule-start-str r)))
            (define after-start (gap-skip-n gb p slen))
            (case tag
              [(block-comment)
               (values (struct-copy scan-state st
                                    [mode 'block-comment]
                                    [rule r]
                                    [block-depth 1])
                       #f after-start)]
              [(here-string)
               (define-values (dw de)
                 (gap-read-delim-word gb after-start))
               (define nl (gap-scan-byte gb de 'forward
                                         (λ (b) (= b #x0A))))
               (define after-nl (if (< nl len) (add1 nl) len))
               (values (struct-copy scan-state st
                                    [mode 'here-string]
                                    [rule r]
                                    [delim dw])
                       #f after-nl)]
              [else (values st #f #f)]))]

       ;; Open bracket
       [(or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
        (values (struct-copy scan-state st
                             [depth (add1 depth)]
                             [stack (cons ch stack)])
                (bracket-face-for-depth depth)
                #f)]

       ;; Close bracket
       [(or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
        (define expected (matching-open ch))
        (cond
          [(and (pair? stack) (char=? (car stack) expected))
           (define new-depth (sub1 depth))
           (values (struct-copy scan-state st
                                [depth new-depth]
                                [stack (cdr stack)])
                   (bracket-face-for-depth new-depth)
                   #f)]
          [else
           (values st bracket-mismatch-face #f)])]

       ;; String quote
       [(and st-obj (char-string-quote? ch st-obj))
        (values (struct-copy scan-state st [mode 'string] [quote-ch ch])
                #f #f)]

       ;; Line comment → skip to end of line
       [(and st-obj (char-comment-start? ch st-obj))
        (define nl (gap-scan-byte gb p 'forward (λ (b) (= b #x0A))))
        (define after-nl (if (< nl len) (add1 nl) len))
        (values st #f after-nl)]

       ;; Escape → skip next char
       [(and st-obj (char-escape? ch st-obj))
        (values st #f (if (< pos1 len) (gap-next-char-pos gb pos1) len))]

       ;; Default — advance past non-bracket char
       [else (values st #f #f)])]

    ;; ── string ──
    [(string)
     (cond
       [(and st-obj (char-escape? ch st-obj))
        (values st #f (if (< pos1 len) (gap-next-char-pos gb pos1) len))]
       [(and st-obj (char-string-quote? ch st-obj))
        (values (struct-copy scan-state st [mode 'normal] [quote-ch #f])
                #f #f)]
       [else (values st #f #f)])]

    ;; ── block-comment ──
    [(block-comment)
     (define end-str   (multi-char-rule-end-str rule))
     (define start-str (multi-char-rule-start-str rule))
     (cond
       [(gap-match-str-at gb p end-str)
        (define new-depth (sub1 block-depth))
        (define end-len (string-length end-str))
        (define after-end (gap-skip-n gb p end-len))
        (if (zero? new-depth)
            (values (struct-copy scan-state st
                                 [mode 'normal] [rule #f] [block-depth 0])
                    #f after-end)
            (values (struct-copy scan-state st [block-depth new-depth])
                    #f after-end))]
       [(and (multi-char-rule-nestable? rule)
             (gap-match-str-at gb p start-str))
        (values (struct-copy scan-state st [block-depth (add1 block-depth)])
                #f (gap-skip-n gb p (string-length start-str)))]
       [else (values st #f #f)])]

    ;; ── here-string ──
    [(here-string)
     (cond
       [(and delim (gap-at-bol? gb p) (gap-match-str-at gb p delim))
        (define delim-end (gap-skip-n gb p (string-length delim)))
        (cond
          [(>= delim-end len)
           (values (struct-copy scan-state st
                                [mode 'normal] [rule #f] [delim #f])
                   #f delim-end)]
          [(char=? (gap-char gb delim-end) #\newline)
           (values (struct-copy scan-state st
                                [mode 'normal] [rule #f] [delim #f])
                   #f (add1 delim-end))]
          [else (values st #f #f)])]
       [else (values st #f #f)])]

    [else (values st #f #f)]))

;; ============================================================
;; scan-chars! — core character scanner loop
;; ============================================================
;; Walks bytes [start, end) tracking parse state.  Emits bracket-face
;; for each bracket char, records checkpoints at interval boundaries.
;; Returns (values final-state final-pos checkpoints-rev emits-rev).

(define (scan-chars! gb start end st rules interval st-obj convergence-check?)
  (define len (gap-length gb))
  (define real-end (min end len))

  (let loop ([p start]
             [s st]
             [cps-rev '()]
             [emits-rev '()]
             [last-cp-pos start])
    (cond
      [(>= p real-end)
       (values s p cps-rev emits-rev)]

      [else
       (define ch   (gap-char gb p))
       (define pos1 (gap-next-char-pos gb p))

       (define-values (next-s emit-face maybe-advance)
         (advance-char gb p pos1 ch s rules st-obj))

       (define next-p (or maybe-advance pos1))

       ;; Emit bracket face
       (define new-emits
         (if emit-face
             (cons (cons emit-face p) emits-rev)
             emits-rev))

       ;; Checkpoint at interval boundary in normal mode
       (define next-mode (scan-state-mode next-s))
       (define cur-mode  (scan-state-mode s))
       (define new-cps
         (if (and (>= p (+ last-cp-pos interval)) (> p start)
                  (eq? cur-mode 'normal))
             (cons (checkpoint p s) cps-rev)
             cps-rev))
       (define new-last-ckpt
         (if (and (>= p (+ last-cp-pos interval)) (> p start)
                  (eq? cur-mode 'normal))
             p last-cp-pos))

       ;; Convergence check — only in normal→normal transitions, no face emitted
       (cond
         [(and convergence-check?
               (not emit-face)
               (eq? cur-mode 'normal)
               (eq? next-mode 'normal)
               (convergence-check? (checkpoint p next-s)))
          (values s p new-cps new-emits)]
         [else
          (loop next-p next-s new-cps new-emits new-last-ckpt)])])))

;; ============================================================
;; bracket-update! — incremental update after buffer change
;; ============================================================

(define (bracket-update! cache gb tp st extent)
  (if st
      (bracket-update-impl! cache gb tp st extent)
      (set-bracket-cache-buf-len! cache (gap-length gb))))

(define (bracket-update-impl! cache gb tp st extent)
  (match-define (cons ext-start ext-end) extent)
  (define new-len (gap-length gb))
  (define delta (- new-len (bracket-cache-buf-len cache)))
  (define interval (bracket-cache-interval cache))
  (define rules (syntax-table-multi-rules st))

  ;; Extend region using same heuristic as font-lock
  (match-define (cons scan-start scan-end)
    (extend-change-region gb ext-start ext-end))

  ;; Partition checkpoints into valid (before scan-start) and stale (after)
  (define old-cps (bracket-cache-checkpoints cache))
  (define-values (valid-cps stale-cps)
    (let part ([cps old-cps] [v '()] [s '()])
      (cond
        [(null? cps) (values (reverse v) (reverse s))]
        [else
         (define cp (car cps))
         (define cpos (checkpoint-pos cp))
         (if (< cpos scan-start)
             (part (cdr cps) (cons cp v) s)
             (part (cdr cps) v
                   (cons (struct-copy checkpoint cp
                            [pos (+ cpos delta)]) s)))])))

  ;; Start state from last valid checkpoint (or initial)
  (define start-st
    (if (pair? valid-cps)
        (checkpoint-state (last valid-cps))
        initial-scan-state))
  (define start-pos
    (if (pair? valid-cps)
        (checkpoint-pos (last valid-cps))
        scan-start))

  ;; Stale convergence list (boxed for mutation during scan)
  (define stale-head (box stale-cps))

  (define (convergence-check? new-cp)
    (define scps (unbox stale-head))
    (cond
      [(null? scps) #f]
      [else
       (define scp (car scps))
       (define spos (checkpoint-pos scp))
       (cond
         [(< spos (checkpoint-pos new-cp))
          (set-box! stale-head (cdr scps))
          (convergence-check? new-cp)]
         [(and (= spos (checkpoint-pos new-cp))
               (equal? (checkpoint-state scp) (checkpoint-state new-cp)))
          #t]
         [(= spos (checkpoint-pos new-cp))
          (set-box! stale-head (cdr scps))
          #f]
         [else #f])]))

  ;; Scan
  (define-values (_final-st _final-pos new-cps-rev raw-emits)
    (scan-chars! gb start-pos scan-end start-st rules interval st
                 convergence-check?))

  ;; Converged tail (remaining stale that matched)
  (define converged-tail (unbox stale-head))

  ;; Combined checkpoint list
  (define new-checkpoints
    (append valid-cps (reverse new-cps-rev) converged-tail))

  ;; Apply bracket-face properties
  (textprop-remove-key! tp scan-start scan-end 'bracket-face)
  (for ([e (in-list (reverse raw-emits))])
    (match-define (cons face-name pos) e)
    (bracket-apply-emit! tp pos face-name))

  ;; Update cache
  (set-bracket-cache-checkpoints! cache new-checkpoints)
  (set-bracket-cache-buf-len! cache new-len))

(define (bracket-rescan-all! cache gb tp st)
  (if st
      (bracket-rescan-all-impl! cache gb tp st)
      (begin
        (set-bracket-cache-checkpoints! cache '())
        (set-bracket-cache-buf-len! cache (gap-length gb)))))

(define (bracket-rescan-all-impl! cache gb tp st)
  (set-bracket-cache-checkpoints! cache '())
  (set-bracket-cache-buf-len! cache 0)

  (define buflen (gap-length gb))
  (when (positive? buflen)
    (textprop-remove-key! tp 0 buflen 'bracket-face)
    (define rules (syntax-table-multi-rules st))
    (define interval (bracket-cache-interval cache))

    (define-values (_st _pos cps-rev emits-rev)
      (scan-chars! gb 0 buflen initial-scan-state rules interval st #f))

    (set-bracket-cache-checkpoints! cache (reverse cps-rev))
    (set-bracket-cache-buf-len! cache buflen)

    (for ([e (in-list (reverse emits-rev))])
      (match-define (cons face-name pos) e)
      (bracket-apply-emit! tp pos face-name))))

;; ============================================================
;; bracket-state-at — query parse state at a byte position
;; ============================================================

(define (bracket-state-at cache gb st pos)
  (and st
       (let* ([cps (bracket-cache-checkpoints cache)]
              [anchor (find-nearest-checkpoint cps pos)]
              [start-p (if anchor (checkpoint-pos anchor) 0)]
              [start-s (if anchor (checkpoint-state anchor) initial-scan-state)])
         (scan-state-to-pos gb st start-p start-s pos))))

(define (find-nearest-checkpoint cps pos)
  (let loop ([best #f] [remaining cps])
    (match remaining
      [(list) best]
      [(list-rest cp rest)
       (if (<= (checkpoint-pos cp) pos)
           (loop cp rest)
           best)])))

(define (scan-state-to-pos gb st start-p start-s target)
  (define rules (syntax-table-multi-rules st))
  (define len (gap-length gb))
  (let loop ([p start-p] [s start-s])
    (cond
      [(>= p target) s]
      [(>= p len) s]
      [else
       (define ch (gap-char gb p))
       (define pos1 (gap-next-char-pos gb p))
       (define-values (next-s _emit-face maybe-advance)
         (advance-char gb p pos1 ch s rules st))
       (define next-p (or maybe-advance pos1))
       (if (> next-p target) #f (loop next-p next-s))])))

;; ============================================================
;; bracket-find-match — find matching bracket
;; ============================================================

(define (bracket-find-match cache gb st pos)
  (and st
       (< pos (gap-length gb))
       (let* ([s (bracket-state-at cache gb st pos)]
              [len (gap-length gb)])
         (and s
              (eq? (scan-state-mode s) 'normal)
              (let ([ch (gap-char gb pos)])
                (cond
                  [(or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
                   (with-handlers ([exn:fail? (λ (_) #f)])
                     (define mp (scan-sexp-forward gb pos len st))
                     (and mp (< mp len)
                          (let ([mc (gap-prev-char-pos gb mp)])
                            (and (>= mc 0) mc))))]
                  [(or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
                   (with-handlers ([exn:fail? (λ (_) #f)])
                     (scan-sexp-backward gb
                                         (add1 (gap-next-char-pos gb pos))
                                         st))]
                  [else #f]))))))
