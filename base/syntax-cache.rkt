#lang racket

;; base/syntax-cache.rkt — Incremental parse-state cache
;;
;; Caches (pos → parse-state) per buffer so that repeated queries
;; (font-lock, matching-paren, forward-sexp) are O(1) after the
;; first scan.  The cache is invalidated on edits and on syntax-table
;; replacement.
;;
;; Edge cases:
;;   - Unterminated string/comment at end of buffer: state reflects it.
;;   - Multi-byte UTF-8: all positions are byte-offsets (gap-buffer convention).
;;   - Nested block comments: comment-depth tracked.
;;   - Heredoc with variable delimiter: stored in scanner state.
;;   - Edit at position 0: entire cache invalidated.
;;   - Query past end of buffer: returns terminal state at ZV.
;;   - Empty/initial cache on first query: scans from BEGV.

(require "../kernel/buffer.rkt"
         "../kernel/gap.rkt"
         "../kernel/syntax.rkt")


;; ============================================================
;; Parse state — immutable snapshot of scanner at a byte position
;; ============================================================
;;
;; Semantics: a parse-state at byte position P represents the
;; scanner state AFTER processing the character at P.
;;   - At P=0: depth=0, not in string or comment.
;;   - At P=1 just after '(': depth=1, open-parens=(1).

(struct parse-state
  (depth              ; paren nesting depth
   in-string?         ; inside a string
   in-comment?        ; inside a comment
   comment-type       ; #f | 'line | 'block | 'heredoc
   comment-nest-depth ; for nested block comments
   open-parens        ; stack of open-paren byte positions
   heredoc-delim      ; active heredoc delimiter string or #f
   syntax-version)    ; syntax-table version at scan time
  #:transparent)

(define initial-parse-state
  (parse-state 0 #f #f #f 0 '() #f 0))

(define (parse-state-top-open-paren ps)
  (and (pair? (parse-state-open-parens ps))
       (car (parse-state-open-parens ps))))

;; ============================================================
;; Per-buffer cache storage
;; ============================================================

;; cache-table: buffer → (hash/c int? parse-state?)
;;   Maps byte position → parse-state at that position.
;;   Sparse: only positions where state was queried or changed.
(define cache-table (make-hasheq))

(define (buffer-cache buf)
  (hash-ref cache-table buf
    (λ ()
      (define h (make-hash))
      ;; Seed with initial state at position 0 (BEGV).
      ;; We use BEGV (which starts at 0) as the base.
      (hash-set! h 0 initial-parse-state)
      (hash-set! cache-table buf h)
      h)))

;; ============================================================
;; Cache lookup helpers
;; ============================================================

;; Find the nearest cached state at position ≤ target-pos.
;; Returns (values cached-pos cached-state).
;; Always succeeds because position 0 is always cached.
(define (find-nearest-cached cache target-pos)
  (for/fold ([best-pos 0]
             [best-state (hash-ref cache 0)])
            ([(pos state) (in-hash cache)]
             #:when (<= pos target-pos)
             #:when (> pos best-pos))
    (values pos state)))

;; ============================================================
;; Scanner internals — mutable state for efficiency during scan
;; ============================================================

;; Mutable scanner state is used DURING a scan to avoid allocating
;; a new parse-state struct at every character.  At the end of the
;; scan, we freeze it into an immutable parse-state for the cache.
(struct scanner-state
  ([depth #:mutable]
   [in-string? #:mutable]
   [in-comment? #:mutable]
   [comment-type #:mutable]
   [comment-nest-depth #:mutable]
   [open-parens #:mutable]
   [heredoc-delim #:mutable])
  #:transparent)

(define (scanner-state-from-parse ps)
  (scanner-state (parse-state-depth ps)
                 (parse-state-in-string? ps)
                 (parse-state-in-comment? ps)
                 (parse-state-comment-type ps)
                 (parse-state-comment-nest-depth ps)
                 (parse-state-open-parens ps)
                 (parse-state-heredoc-delim ps)))

(define (scanner-state->parse ss syntax-version)
  (parse-state (scanner-state-depth ss)
               (scanner-state-in-string? ss)
               (scanner-state-in-comment? ss)
               (scanner-state-comment-type ss)
               (scanner-state-comment-nest-depth ss)
               (scanner-state-open-parens ss)
               (scanner-state-heredoc-delim ss)
               syntax-version))

;; ============================================================
;; Gap-buffer helpers
;; ============================================================

(define (char-at gb pos)
  (let-values ([(ch _len) (gap-char-at gb pos)]) ch))

(define (char-len gb pos)
  (let-values (([_ch len] (gap-char-at gb pos))) len))

(define (skip-n gb pos n)
  (let loop ([p pos] [i n])
    (if (zero? i) p (loop (+ p (char-len gb p)) (sub1 i)))))

;; Does string s match at byte position pos in gap-buffer gb (up to len)?
(define (match-str-at gb pos buflen s)
  (define slen (string-length s))
  (and (<= (+ pos slen) buflen)
       (let loop ([i 0] [p pos])
         (if (= i slen)
             #t
             (let-values ([(ch _cl) (gap-char-at gb p)])
               (and (char=? ch (string-ref s i))
                    (loop (add1 i) (+ p (char-len gb p)))))))))

;; Is pos at the beginning of a line?
(define (at-bol? gb pos)
  (or (zero? pos)
      (let ([prev (gap-prev-char-pos gb pos)])
        (and prev (char=? (char-at gb prev) #\newline)))))

;; Read a non-whitespace word starting at pos; returns (values word-str end-pos).
(define (read-delim-word gb pos buflen)
  (let loop ([p pos] [chars '()])
    (if (>= p buflen)
        (values (list->string (reverse chars)) p)
        (let*-values ([(ch _cl) (gap-char-at gb p)])
          (if (or (char=? ch #\space) (char=? ch #\tab)
                  (char=? ch #\newline) (char=? ch #\return))
              (values (list->string (reverse chars)) p)
              (loop (+ p (char-len gb p)) (cons ch chars)))))))

;; ============================================================
;; Core scan — advance one semantic unit, update scanner state
;; ============================================================
;;
;; Returns the next byte position to process, or pos unchanged if stuck.

(define (scan-one-step! gb pos ss st multi-rules buflen)
  (define ch (char-at gb pos))
  (define pos1 (+ pos (char-len gb pos)))

  (define (next pos) pos)           ; advance helper
  (define (advance) pos1)
  (define (advance-n n) (skip-n gb pos n))

  ;; ── find multi-char rule whose start matches at pos ──
  (define (match-multi)
    (and multi-rules
         (for/or ([r (in-list multi-rules)])
           (and (match-str-at gb pos buflen (multi-char-rule-start r))
                r))))
  (define (match-end-of rule)
    (and (multi-char-rule-end rule)
         (match-str-at gb pos buflen (multi-char-rule-end rule))))

  (cond
    ;; ============================================================
    ;; NORMAL state
    ;; ============================================================
    [(not (or (scanner-state-in-string? ss)
              (scanner-state-in-comment? ss)))
     ;; Check multi-char rule start before single-char syntax checks
     (define matched (match-multi))
     (cond
       [matched
        (set-scanner-state-in-comment?! ss #t)
        (set-scanner-state-comment-type! ss (multi-char-rule-tag matched))
        (define start-len (string-length (multi-char-rule-start matched)))
        (define after-start (advance-n start-len))
        (cond
          [(multi-char-rule-delim-capture? matched)
           ;; heredoc: #<<DELIM — capture delimiter word, skip to next line
           (let*-values ([(delim delim-end)
                          (read-delim-word gb after-start buflen)]
                         [(nl) (gap-scan-forward-byte
                                gb delim-end (curry = #x0A))])
             (set-scanner-state-heredoc-delim! ss delim)
             (set-scanner-state-comment-nest-depth! ss 1)
             (if (< nl buflen) (add1 nl) buflen))]
          [(multi-char-rule-nestable? matched)
           (set-scanner-state-comment-nest-depth! ss 1)
           after-start]
          [else
           (set-scanner-state-comment-nest-depth! ss 1)
           after-start])]
       [(char-open? ch st)
        (set-scanner-state-open-parens! ss
          (cons pos (scanner-state-open-parens ss)))
        (set-scanner-state-depth! ss (add1 (scanner-state-depth ss)))
        (advance)]
       [(char-close? ch st)
        (set-scanner-state-depth! ss
          (max 0 (sub1 (scanner-state-depth ss))))
        (when (pair? (scanner-state-open-parens ss))
          (set-scanner-state-open-parens! ss
            (cdr (scanner-state-open-parens ss))))
        (advance)]
       [(char-string-quote? ch st)
        (set-scanner-state-in-string?! ss #t)
        (advance)]
       [(char-comment-start? ch st)
        ;; Line comment: skip to end of line
        (set-scanner-state-in-comment?! ss #t)
        (set-scanner-state-comment-type! ss 'line)
        (set-scanner-state-comment-nest-depth! ss 1)
        (define nl (gap-scan-forward-byte gb pos (curry = #x0A)))
        ;; After consuming the line comment, exit comment state.
        ;; But we need to exit at the newline, not skip past it.
        (define end-pos (min nl buflen))
        (set-scanner-state-in-comment?! ss #f)
        (set-scanner-state-comment-type! ss #f)
        (set-scanner-state-comment-nest-depth! ss 0)
        (if (< nl buflen) (add1 nl) buflen)]
       [else (advance)])]

    ;; ============================================================
    ;; STRING state
    ;; ============================================================
    [(scanner-state-in-string? ss)
     (cond
       [(char-escape? ch st)
        ;; Skip escape + next char
        (if (< pos1 buflen) (advance-n 2) buflen)]
       [(char-string-quote? ch st)
        ;; End of string
        (set-scanner-state-in-string?! ss #f)
        (advance)]
       [else (advance)])]

    ;; ============================================================
    ;; COMMENT state (block-comment or heredoc)
    ;; ============================================================
    [else
     (define ctype (scanner-state-comment-type ss))
     (cond
       ;; ── heredoc ──
       [(eq? ctype 'heredoc)
        (define delim (scanner-state-heredoc-delim ss))
        (cond
          [(and delim (at-bol? gb pos)
                (match-str-at gb pos buflen delim))
           (define delim-end (advance-n (string-length delim)))
           (cond
             [(>= delim-end buflen)
              (set-scanner-state-in-comment?! ss #f)
              (set-scanner-state-comment-type! ss #f)
              (set-scanner-state-comment-nest-depth! ss 0)
              (set-scanner-state-heredoc-delim! ss #f)
              delim-end]
             [(char=? (char-at gb delim-end) #\newline)
              (set-scanner-state-in-comment?! ss #f)
              (set-scanner-state-comment-type! ss #f)
              (set-scanner-state-comment-nest-depth! ss 0)
              (set-scanner-state-heredoc-delim! ss #f)
              (add1 delim-end)]
             [else (advance)])]
          [else (advance)])]

       ;; ── block-comment (nestable) ──
       [else
        (define matched-start (match-multi))
        (define matched-end
          (and (not matched-start)
               (let loop ([rules multi-rules])
                 (and (pair? rules)
                      (let ([r (car rules)])
                        (if (eq? (multi-char-rule-tag r) ctype)
                            (match-end-of r)
                            (loop (cdr rules))))))))
        (cond
          [(and matched-end
                (= (scanner-state-comment-nest-depth ss) 1))
           ;; Exit outermost block comment
           (set-scanner-state-comment-nest-depth! ss
             (sub1 (scanner-state-comment-nest-depth ss)))
           (when (zero? (scanner-state-comment-nest-depth ss))
             (set-scanner-state-in-comment?! ss #f)
             (set-scanner-state-comment-type! ss #f))
           (advance-n (string-length (multi-char-rule-end
                                       (for/first ([r (in-list multi-rules)]
                                                   #:when (eq? (multi-char-rule-tag r) ctype))
                                         r))))]
          [matched-end
           ;; Exit one level of nested block comment
           (set-scanner-state-comment-nest-depth! ss
             (sub1 (scanner-state-comment-nest-depth ss)))
           (advance-n (string-length (multi-char-rule-end
                                       (for/first ([r (in-list multi-rules)]
                                                   #:when (eq? (multi-char-rule-tag r) ctype))
                                         r))))]
          [matched-start
           ;; Nested block comment start
           (set-scanner-state-comment-nest-depth! ss
             (add1 (scanner-state-comment-nest-depth ss)))
           (advance-n (string-length (multi-char-rule-start matched-start)))]
          [else (advance)])])]))

;; ============================================================
;; Scan from `start-pos` with `start-state` up to `target-pos`
;; ============================================================
;;
;; Returns the parse-state at target-pos.
;; Side-effect: populates cache at target-pos (and possibly
;; intermediate positions for important state changes).

(define (scan-to! gb start-pos ss st multi-rules buflen target-pos cache syntax-version)
  ;; If we're already past target, no scan needed.
  (if (>= start-pos target-pos)
      (scanner-state->parse ss syntax-version)
      (let loop ([pos start-pos])
        (if (>= pos target-pos)
            ;; Reached target: freeze state and cache it
            (let ([ps (scanner-state->parse ss syntax-version)])
              (hash-set! cache target-pos ps)
              ps)
            (let ([next-pos (scan-one-step! gb pos ss st multi-rules buflen)])
              (if (= next-pos pos)
                  ;; Stuck (e.g., pos >= buflen): freeze and return
                  (let ([ps (scanner-state->parse ss syntax-version)])
                    (hash-set! cache pos ps)
                    ps)
                  (loop next-pos)))))))

;; ============================================================
;; buffer-parse-state — main public query
;; ============================================================
;;
;; Returns the parse-state at byte position `pos` in `buf`.
;; Uses the cache when possible; scans incrementally when needed.
;; Caches the result so subsequent queries are O(1).

(define (buffer-parse-state buf pos)
  (define st (buffer-syntax-table buf))
  (define gb (buffer-gap buf))
  (define buflen (gap-byte-length gb))
  (define target (min pos buflen))
  (define sv (buffer-syntax-version buf))
  (define cache (buffer-cache buf))

  ;; Find nearest cached position ≤ target with matching syntax-version.
  (define-values (cached-pos cached-ps)
    (for/fold ([best-pos 0]
               [best-ps (hash-ref cache 0)])
              ([(p ps) (in-hash cache)]
               #:when (<= p target)
               #:when (> p best-pos)
               #:when (= (parse-state-syntax-version ps) sv))
      (values p ps)))

  ;; If the cached state has a stale syntax-version, invalidate it
  ;; and re-scan from position 0.
  (define (valid-start? p ps) (= (parse-state-syntax-version ps) sv))

  (cond
    [(and (valid-start? cached-pos cached-ps) (= cached-pos target))
     ;; Exact hit in cache
     cached-ps]
    [(valid-start? cached-pos cached-ps)
     ;; Scan forward from cached-pos to target
     (define ss (scanner-state-from-parse cached-ps))
     (define multi-rules (and st (syntax-table-multi-rules st)))
     (scan-to! gb cached-pos ss st multi-rules buflen target cache sv)]
    [else
     ;; Cache stale: rescan from 0
     (define ss (scanner-state
                  0 #f #f #f 0 '() #f))
     (define multi-rules (and st (syntax-table-multi-rules st)))
     (scan-to! gb 0 ss st multi-rules buflen target cache sv)]))

;; ============================================================
;; Invalidation
;; ============================================================

(define (syntax-cache-invalidate! buf [from-pos 0])
  (define cache (hash-ref cache-table buf (λ () #f)))
  (when cache
    (for ([(pos _) (in-hash cache)] #:when (>= pos from-pos))
      (hash-remove! cache pos))
    ;; Ensure position 0 always has an entry (re-seed initial state)
    (unless (hash-has-key? cache 0)
      (hash-set! cache 0 initial-parse-state))))

(define (syntax-cache-reset! buf)
  (define cache (hash-ref cache-table buf (λ () #f)))
  (when cache
    (hash-clear! cache)
    (hash-set! cache 0 initial-parse-state)))

;; ============================================================
;; Cleanup
;; ============================================================

(define (syntax-cache-buffer-cleanup! buf)
  (hash-remove! cache-table buf))

;; ============================================================
;; Matching paren
;; ============================================================

(define (matching-paren buf [pos (buffer-point buf)])
  (define st (buffer-syntax-table buf))
  (unless st (error 'matching-paren "no syntax table for buffer"))

  (define gb (buffer-gap buf))
  (define buflen (gap-byte-length gb))
  (define ch (and (< pos buflen) (char-at gb pos)))

  (cond
    [(and ch (char-open? ch st))
     ;; Scan forward from pos+1 to find matching close paren
     (define ss (scanner-state
                  1  ; depth starts at 1 (we're inside the open paren)
                  #f #f #f 0 '() #f))
     (define multi-rules (syntax-table-multi-rules st))
     (define sv (buffer-syntax-version buf))
     (let loop ([p (+ pos (char-len gb pos))])
       (if (>= p buflen)
           #f  ; no matching close paren found
           (let ([next-p (scan-one-step! gb p ss st multi-rules buflen)])
             (cond
               [(= next-p p) #f]  ; stuck
               [(and (zero? (scanner-state-depth ss))
                     (not (scanner-state-in-string? ss))
                     (not (scanner-state-in-comment? ss)))
                ;; Found the matching close paren: it's at (next-p - char-len)
                (let*-values ([(ch2 _cl) (gap-char-at gb (max 0 (- next-p 4)))])
                  ;; Walk back to find the close-paren char just before next-p.
                  ;; scan-one-step! already consumed it, so it's at (prev-char-pos next-p).
                  (gap-prev-char-pos gb next-p))]
               [else (loop next-p)]))))]

    [(and ch (char-close? ch st))
     ;; Query state at position just BEFORE this close paren.
     ;; State at `pos` would already have processed the close paren
     ;; and popped it from the open-parens stack.
     (define prev (gap-prev-char-pos gb pos))
     (define ps (buffer-parse-state buf (or prev 0)))
     (parse-state-top-open-paren ps)]

    [else #f]))

;; ============================================================
;; Module exports (at end so all definitions are visible)
;; ============================================================

(provide
 ;; parse-state
 parse-state? parse-state
 parse-state-depth parse-state-in-string? parse-state-in-comment?
 parse-state-comment-type parse-state-comment-nest-depth
 parse-state-open-parens parse-state-top-open-paren
 parse-state-syntax-version
 initial-parse-state

 ;; query
 buffer-parse-state

 ;; invalidation
 syntax-cache-invalidate!
 syntax-cache-reset!

 ;; cleanup
 syntax-cache-buffer-cleanup!

 ;; navigation
 matching-paren)
