#lang racket

;; ============================================================================
;; lang/bracket-cache.rkt — Bracket depth coloring with checkpoint caching
;; ============================================================================
;;
;; ── Problem ───────────────────────────────────────────────────────────────
;; Show each pair of matching brackets ( () [] {} ) in a different color based
;; on nesting depth, Emacs "rainbow parens" style.  Mismatched / unbalanced
;; brackets get an error face (white on red background).
;;
;; The hard part is making this fast: a naive re-scan of the entire buffer on
;; every keystroke is O(file) per edit and visibly lags on large files.  We
;; need incremental update that is usually O(Δ) in practice.
;;
;;
;; ── Algorithm ─────────────────────────────────────────────────────────────
;; Checkpoint-based re-scan with equality convergence.
;;
;; 1.  Store a snapshot of the scanner state (depth, stack, string/comment
;;     mode, etc.) every N bytes along the buffer — a "checkpoint".
;;
;; 2.  On each edit:
;;     a) Partition checkpoints into two sets:
;;        - valid:   positions before the edit region.  These are untouched.
;;        - stale:   positions at or after the edit.  Shift their recorded
;;                   positions by the buffer-length delta.
;;     b) Restart scanning from the last valid checkpoint (or position 0)
;;        forward through the changed region.
;;     c) As we scan, emit bracket-face text-properties for each bracket char
;;        and record new checkpoints.
;;     d) Whenever we reach a position that matches a shifted stale checkpoint,
;;        compare the CURRENT scanner state against the STALE checkpoint's
;;        state with equal?:
;;          - equal? → CONVERGED.  The rest of the buffer is unchanged.
;;            Stop scanning, keep the stale tail.
;;          - not equal? → the edit changed bracket semantics downstream
;;            (e.g. inserted an open-paren that shifts all depth by 1).
;;            Continue scanning.
;;
;; Convergence is the key insight.  For most edits — typing a character, word,
;; or line — the bracket structure stays the same.  We converge at the first
;; stale checkpoint past the edit, so cost ≈ O(interval + edit-size).  Only
;; edits that actually change bracket nesting (inserting/deleting a paren,
;; adding a string that spans former brackets, etc.) trigger a full re-scan
;; to end-of-buffer — which is exactly when a re-scan is semantically
;; necessary.
;;
;;
;; ── Why this approach, not alternatives ───────────────────────────────────
;;
;; Rejected: full bracket tree in kernel.
;;   A BST or interval tree of matched-bracket-pairs would give O(log n)
;;   update but requires maintaining a second data structure that mirrors the
;;   gap buffer, with manual pointer adjustments on every insert/delete.
;;   Error-prone and heavy for a feature that runs every keystroke.
;;
;; Rejected: parser-cache (parse-state at each line).
;;   Most Lisp parsers naturally produce parse-state at line boundaries.
;;   But we don't have a full Lisp parser, and building one just for bracket
;;   coloring is overkill.  Also, deeply nested structures (long let* body)
;;   would have NO checkpoint for thousands of lines, degrading to rescan
;;   from BOF for any interior edit.
;;
;; Rejected: skip-list of bracket positions.
;;   Tracking only bracket positions (not parse state) means any edit that
;;   changes depth requires updating ALL subsequent positions.  O(n) update.
;;
;; Why checkpoints at fixed intervals rather than at depth-0 ("top-level"):
;;   Depth-0 positions are natural anchors (nothing is nested), but there is
;;   no guarantee how frequently they occur.  In a single top-level definition
;;   spanning 2000 lines, there are zero depth-0 positions.  Fixed intervals
;;   guarantee bounded re-scan cost regardless of nesting shape.
;;
;; Checkpoint interval = 1024 bytes.  For a 1 MB file this is ~1000
;; checkpoints.  Memory: each scan-state is a small struct + a persistent
;; list (stack), shared between checkpoints via struct-copy.  Negligible.
;;
;;
;; ── key independence: font-lock vs bracket ───────────────────────────────
;;
;; font-lock  writes to text-property key 'face (string, comment, keyword).
;; bracket-cache writes to text-property key 'bracket-face (depth coloring).
;;
;; They never touch each other's keys.  Both engines call textprop-remove-key!
;; which removes only the specified key, leaving the other intact.
;;
;; Render priority: 'face (font-lock) > 'bracket-face.
;; This means brackets inside strings/comments — which bracket-cache's state
;; machine correctly skips — also get the string/comment face from font-lock,
;; which overrides any stale bracket face.  Double safety.
;;
;;
;; ── Edge cases handled ────────────────────────────────────────────────────
;;
;; 1. Empty buffer.
;;    rescan-all! → 0 checkpoints, no emit.  Subsequent bracket-update! with
;;    extent (0 . 0) from insert: starts from initial state at pos 0.
;;
;; 2. No brackets in buffer.
;;    Normal scanning produces no emits.  Checkpoints are still recorded at
;;    interval boundaries (every 1024 bytes), so edits are fast.
;;
;; 3. Mismatched brackets: "(]" or ")" at depth 0 (extra close).
;;    Emits bracket-mismatch-face for the offending char.  Does NOT crash
;;    or enter an invalid state.  The stack is not popped on mismatch, and
;;    depth is not changed, so subsequent brackets continue coloring based
;;    on the actual (pre-mismatch) nesting.
;;
;;    Consequence: after a mismatch, all downstream brackets may be at the
;;    "wrong" depth (depth is the count of UNMATCHED opens, which includes
;;    the one before the mismatch).  This is a feature: it makes the
;;    structural error visible.
;;
;; 4. nil syntax-table (e.g. Python buffer with no syntax-table).
;;    bracket-update! and bracket-rescan-all! are no-ops.  They only update
;;    buf-len to stay in sync.  bracket-state-at and bracket-find-match
;;    return #f.
;;
;; 5. Buffer with only strings/comments, no brackets.
;;    Scanner correctly tracks mode transitions.  No bracket-face emitted.
;;    Checkpoints still recorded at interval boundaries in normal mode
;;    (i.e., outside strings/comments).
;;
;; 6. Heredoc (#<<HERE ... HERE) spanning pages.
;;    advance-char handles heredoc start, delimiter capture, and heredoc-end
;;    detection at BOL.  mode stays 'here-string the entire time, so no
;;    bracket faces emitted inside heredoc body.
;;
;; 7. Nestable block comments (#| ... #| inner |# ... |#).
;;    block-depth counter tracks nesting.  Exits only when block-depth→0.
;;    No bracket faces emitted inside any level of block comment.
;;
;; 8. Escaped chars in strings: "\"" or "\\".
;;    \ is classified as 'escape by the syntax-table.  When encountered
;;    in string mode, the next character is skipped entirely (via
;;    maybe-advance → gap-next-char-pos).  The " after \ does NOT close
;;    the string.
;;
;; 9. Expression prefix: ' '(a b c).
;;    The ' is classified as 'expression-prefix.  We do NOT skip it or
;;    treat the following ( as special.  The ( is colored normally as
;;    a bracket.  This is intentional: it IS a bracket, just semantically
;;    a quote form.  The coloring still correctly reflects nesting.
;;    (Emacs does the same.)
;;
;; 10. Character literals: #\(  #\newline.
;;    The \ after # is an escape character.  In normal mode, escape skips
;;    the next character.  So #\( is processed as: #→advance, \→escape-skip,
;;    then the ( after \ is skipped and never seen as a bracket.
;;    This prevents false bracket detection in char literals.
;;
;; 11. Multiple edits batched in one command at different positions.
;;    dirty.rkt merges all change extents into a single (min-start . max-end).
;;    bracket-update! uses total delta (new-len - old-len) to shift stale
;;    checkpoints.  This is an approximation: if edits are at different
;;    positions, the single-delta shift may be wrong for some checkpoints.
;;    However, the convergence-check is the safety net — wrong shifts cause
;;    convergence to FAIL, leading to a correct (but slower) re-scan to EOF.
;;    In the common case (single edit per keystroke), delta is exact.
;;
;; 12. Grouping by paren type: () [] {}.
;;    The stack stores the actual open-bracket character (not just depth).
;;    This enables mismatch detection when types don't match, e.g. "(]".
;;    [{]} is correctly colored as: { depth-0, [ depth-1, } mismatch, ] depth-1.
;;
;;
;; ── Known limitations & future risks ─────────────────────────────────────
;;
;; A. O(n) convergence on structural edits.
;;    When an edit changes bracket nesting for the entire file (e.g.
;;    inserting an open-paren at buffer start), the stale checkpoints
;;    will ALL have wrong depth, so convergence fails at every one, causing
;;    a full file re-scan.  This is O(file) — correct but slow for huge
;;    files.  A future optimization could add a "force re-color from next
;;    top-level form" heuristic that detects this case and jumps forward.
;;
;; B. Shift approximation for multi-edit batches.
;;    When one command produces edits at different positions (e.g. a macro
;;    that inserts text in two places), dirty.rkt merges them into one
;;    extent.  The single-delta shift may be slightly off for checkpoints
;;    between the edit locations.  Convergence safety net handles this
;;    (wrong shift → failed convergence → scan continues), but at the cost
;;    of a longer scan.  Fixing this would require dirty.rkt to report
;;    per-edit positions rather than a merged extent.
;;
;; C. No checkpointing inside strings or comments.
;;    Checkpoints are only recorded when mode is 'normal.  This means long
;;    strings/comments have no interior checkpoints.  An edit inside a
;;    string must rescan from the last checkpoint before the string started.
;;    For very long strings (e.g. a 50KB heredoc), this means re-scanning
;;    50KB of string content for every keystroke inside it — O(string-length)
;;    per edit instead of O(interval).  This is rare in practice (strings
;;    that long usually aren't edited interactively), but could be addressed
;;    by also checkpointing in string/comment mode with the appropriate
;;    mode-specific state.
;;
;; D. Clone of font-lock's state machine.
;;    advance-char duplicates the mode-switching logic from font-lock's
;;    syntax-scan! (string detection, block-comment, heredoc, escapes).
;;    If font-lock's rules change (new syntax-table constructs), both places
;;    need updating.  A future unification could extract a shared
;;    "syntax-step" that both engines call, returning (next-mode emit-faces).
;;    For now this is acceptable because the logic is ~100 lines and stable.
;;
;; E. bracket-face property uses add1 for byte position.
;;    bracket-apply-emit! sets the interval as [pos, pos+1).  All standard
;;    brackets (() [] {}) are single-byte ASCII, so this is always correct.
;;    If the language ever has multi-byte bracket characters (e.g. 「」 in
;;    CJK), this would need to use gap-next-char-pos instead of add1.
;;    Currently not an issue for any supported language (Racket, Scheme,
;;    Python).
;;
;; F. No face-id caching for bracket faces.
;;    The render layer calls face-id-with-overlay for each glyph, which
;;    does a hash lookup.  For bracket-heavy files, this could be a hot
;;    path.  A future optimization: pre-resolve bracket-face symbols to
;;    face-ids at scan time and store the face-id in text-properties
;;    instead of face symbols.  Requires exposing face-cache to this module
;;    (currently it only knows face names).
;;
;; G. No tracking of per-edit convergence rate.
;;    We don't measure how often convergence succeeds vs fails.  Adding
;;    instrumentation here would help identify pathological cases in
;;    real-world usage (e.g. if a particular editing pattern causes
;;    frequent full re-scans).
;;
;; H. Convergence depends on equal? of scan-state.
;;    scan-state is #:transparent and contains only numbers, chars, symbols,
;;    strings, and lists — all values with structural equal? semantics.
;;    If a future change adds a field with non-structural equality (e.g.
;;    a procedure or a struct without #:transparent), convergence would
;;    silently break (always return #f, always re-scan to EOF).  The
;;    symptom would be gradual slowdown, not visible breakage.
;;
;; I. textprop-remove-key! iterates ALL intervals.
;;    Currently O(total-intervals) per bracket-update! call, because it
;;    filters all intervals for those with the target key.  For files with
;;    many small font-lock intervals (e.g. keyword-highlighted tokens),
;;    this could be slow.  An optimization: use interval-map-ref/bounds
;;    to walk only the affected range.  Low priority unless profiling
;;    shows it as a hotspot.
;;
;;
;; ── Dependencies ─────────────────────────────────────────────────────────
;;   kernel/data/gap.rkt        — gap buffer byte access
;;   kernel/data/query.rkt      — gap-char, gap-next-char-pos, gap-match-str-at, etc.
;;   kernel/data/textprop.rkt   — text-properties (interval-map)
;;   kernel/data/syntax.rkt     — syntax-table, multi-char-rule
;;   kernel/motion.rkt          — scan-sexp-forward/backward (used by find-match)
;;   display/face.rkt           — define-face!, make-face-attrs
;;   lang/font-lock.rkt         — extend-change-region (region extension heuristic)
;; ============================================================================

(require "../kernel/data/gap.rkt"
         "../kernel/data/query.rkt"
         "../kernel/data/textprop.rkt"
         "../kernel/data/syntax.rkt"
         "../kernel/motion.rkt"
         "../display/face.rkt"
         "font-lock.rkt")

(provide
 bracket-cache? bracket-cache-checkpoints bracket-cache-buf-len bracket-cache-interval
 make-bracket-cache
 bracket-update!
 bracket-rescan-all!
 bracket-state-at
 bracket-find-match
 bracket-register-faces!)

;; ============================================================================
;; Data structures
;; ============================================================================

;; scan-state — immutable snapshot of the scanner at a position.
;;
;; Captures everything needed to resume scanning from this point and produce
;; identical results: current nesting depth, bracket stack for mismatch
;; detection, and the mode stack for string/comment/heredoc skipping.
;;
;; All fields are plain Racket values (numbers, chars, symbols, lists,
;; structs).  equal? works structurally.  This is essential for convergence
;; checking: we compare new scan-state against a stale checkpoint's state
;; with equal?, and it must return #t iff the scan would produce identical
;; downstream results.

(struct scan-state
  (depth        ; nonnegative-integer — # of unmatched open brackets before here
   mode         ; symbol — 'normal | 'string | 'block-comment | 'here-string
   quote-ch     ; (or/c char? #f) — the quote that opened this string
   rule         ; (or/c multi-char-rule? #f) — active block-comment or heredoc rule
   block-depth  ; integer — nesting depth within nestable block-comments
   delim        ; (or/c string? #f) — heredoc delimiter word
   stack)        ; (listof char?) — unmatched open brackets, innermost at head.
                ;   e.g. depth=3, stack='(#\{ #\[ #\() means currently inside
                ;   "{ [ (" and expecting ) ] } to close.
  #:transparent)

(define initial-scan-state
  (scan-state 0 'normal #f #f 0 #f '()))

;; checkpoint — a scan-state snapshot at a byte position.
;;
;; pos is the byte offset BEFORE consuming the character at pos.
;; The state reflects everything up to but not including pos.
;; This is the standard "parser state before token" convention.

(struct checkpoint (pos state) #:transparent)

;; bracket-cache — mutable store of checkpoints plus metadata.
;;
;; checkpoints: strictly ascending list by pos.  A list (not vector) because
;;   we frequently partition (takef + dropf) and append tails.
;; buf-len: gap-length at completion of the last full scan or update.
;;   Used to compute the delta for shifting stale checkpoints.
;; interval: bytes between checkpoints.  1024 is a good default — balances
;;   memory (~1K checkpoints per MB) against re-scan cost (~1KB per edit).

(struct bracket-cache
  ([checkpoints #:mutable]
   [buf-len     #:mutable]
   interval)
  #:transparent)

(define (make-bracket-cache [interval 1024])
  (bracket-cache '() 0 interval))

;; ============================================================================
;; Face names
;; ============================================================================

;; 6 colors cycling by depth modulo 6.  Chosen for distinguishability on
;; dark backgrounds.  More than 6 levels is rarely useful — most code
;; doesn't nest brackets deeper than 6, and beyond that distinguishing
;; colors becomes hard anyway.  The vector wrap-around is cheap.

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

;; ============================================================================
;; bracket-register-faces! — called once at startup from main.rkt
;; ============================================================================
;;
;; Registers 7 face names in the global face-cache:
;;   6 depth colors (foreground only, cycling)
;;   1 mismatch face (white on red background)
;;
;; These are defined with concrete RGB values (not symbolic colors) so they
;; look consistent across terminals.  All assume dark-background terminals
;; — this is Emacs convention.  Light-background support would need a
;; separate palette.

(define (bracket-register-faces!)
  (define colors
    (list (list 255 180 0)   ; gold
          (list 180 120 255) ; purple
          (list 80  200 255) ; cyan
          (list 255 100 100) ; red
          (list 100 255 100) ; green
          (list 255 200 80))); orange
  (for ([(c i) (in-indexed (in-list colors))])
    (define name (vector-ref bracket-depth-faces i))
    (define-face! name (make-face-attrs 'foreground c)))
  (define-face! bracket-mismatch-face
                (make-face-attrs 'foreground (list 255 255 255)
                                 'background (list 180 0 0))))

;; ============================================================================
;; Bracket matching helpers
;; ============================================================================

(define (matching-close ch)
  (case ch [(#\() #\)] [(#\[) #\]] [(#\{) #\}] [else #\)]))

(define (matching-open ch)
  (case ch [(#\)) #\(] [(#\]) #\[] [(#\}) #\{] [else #\(]))

;; ============================================================================
;; bracket-apply-emit! — write bracket-face for a single bracket character
;; ============================================================================
;;
;; All standard bracket characters () [] {} are single-byte ASCII, so the
;; text-property interval is always [pos, pos+1).  If multi-byte bracket
;; characters are ever supported (e.g. CJK brackets), this needs to use
;; gap-next-char-pos instead of add1.  See limitation E in the module
;; header.

(define (bracket-apply-emit! tp pos face-name)
  (define pos2 (add1 pos))
  (when (< pos pos2)
    (textprop-put! tp pos pos2 'bracket-face face-name)))

;; ============================================================================
;; advance-char — single character state transition
;; ============================================================================
;;
;; Pure function: given a scan-state `st`, the current character `ch` at byte
;; position `p`, and the syntax-table, returns:
;;   (values next-state emit-face? override-next-pos?)
;;
;; override-next-pos:
;;   #f          → caller advances to pos1 (gap-next-char-pos gb p)
;;   non-#f      → caller jumps to this position instead
;;
;; This handles multi-character constructs in a single step:
;;   - "|#" block-comment close (2 chars) → jump past both
;;   - "; ... \n" line comment (variable) → jump past newline
;;   - "\\" escape in string (2 chars) → jump past both
;;   - "#<<HERE ... HERE" heredoc (pages) → jump past delimiter line
;;
;; The function does NOT emit bracket-face for non-bracket positions.
;; It only emits for positions where the character IS a bracket AND the
;; current mode is 'normal.  Inside strings/comments, brackets are
;; silently advanced past.
;;
;; IMPORTANT: this function duplicates logic from font-lock.rkt's
;; syntax-scan!.  See limitation D in the module header.
;;
;; st-obj: the syntax-table object — only used for character classification
;;   (char-string-quote?, char-escape?, char-comment-start?).  Could be
;;   eliminated if rules encoded these checks, but currently needed.

(define (advance-char gb p pos1 ch st rules st-obj)
  (match-define (scan-state depth mode quote-ch rule block-depth delim stack) st)
  (define len (gap-length gb))

  (case mode
    ;; ── normal ──────────────────────────────────────────────────────────
    ;; The common case.  We check for multi-char rule starts FIRST because
    ;; the first character of a rule (e.g. '#' in "#|") may be classified
    ;; as 'word by the syntax-table.  Rule priority prevents treating "#|"
    ;; as word-char '#' + pipe '|'.
    [(normal)
     (cond
       ;; Multi-char rule start (block-comment, heredoc)
       [(for/or ([r (in-list rules)])
          (and (gap-match-str-at gb p (multi-char-rule-start-str r)) r))
        => (λ (r)
            (define tag (multi-char-rule-tag r))
            (define slen (string-length (multi-char-rule-start-str r)))
            (define after-start (gap-skip-n gb p slen))
            (case tag
              [(block-comment)
               ;; Enter block-comment mode.  Nesting starts at 1.
               ;; All subsequent chars are skipped until block-depth→0
               ;; when matching end-str is found.
               (values (struct-copy scan-state st
                                    [mode 'block-comment]
                                    [rule r]
                                    [block-depth 1])
                       #f after-start)]
              [(here-string)
               ;; Capture the delimiter word (e.g. "HERE" in "#<<HERE"),
               ;; skip to end of line.  Subsequent lines are in
               ;; here-string mode until the delimiter appears at BOL.
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

       ;; Open bracket — push to stack, increment depth, emit face.
       ;; Face is based on the depth BEFORE incrementing, so the
       ;; outermost open-paren is depth-0, its contents are depth-1, etc.
       [(or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
        (values (struct-copy scan-state st
                             [depth (add1 depth)]
                             [stack (cons ch stack)])
                (bracket-face-for-depth depth)
                #f)]

       ;; Close bracket — compare against stack top.
       ;; Match → pop, decrement depth, emit face at new depth.
       ;; Mismatch → emit mismatch face, don't change depth/stack.
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
           ;; Mismatch: this close doesn't match the expected open.
           ;; Don't change state — the error is visible but doesn't
           ;; corrupt subsequent coloring.
           (values st bracket-mismatch-face #f)])]

       ;; String quote (") → enter string mode.
       ;; All characters until the matching close quote are skipped,
       ;; including brackets, without emitting faces.
       [(and st-obj (char-string-quote? ch st-obj))
        (values (struct-copy scan-state st [mode 'string] [quote-ch ch])
                #f #f)]

       ;; Line comment (;) → skip to end of line.
       ;; Entire comment is hopped in one step via gap-scan-byte.
       ;; This avoids O(line-length) per-char stepping through comments.
       [(and st-obj (char-comment-start? ch st-obj))
        (define nl (gap-scan-byte gb p 'forward (λ (b) (= b #x0A))))
        (define after-nl (if (< nl len) (add1 nl) len))
        (values st #f after-nl)]

       ;; Escape (\) → skip next char.
       ;; Handles #\(, #\newline in Racket.  Also handles escaped chars
       ;; in normal mode (rare but possible in some syntax tables).
       [(and st-obj (char-escape? ch st-obj))
        (values st #f (if (< pos1 len) (gap-next-char-pos gb pos1) len))]

       ;; Default: word char, whitespace, expression prefix, etc.
       ;; Just advance without emitting any face.
       [else (values st #f #f)])]

    ;; ── string ──────────────────────────────────────────────────────────
    ;; Inside a string literal (e.g. "hello (world)").
    ;; No bracket face is emitted for any character inside.
    ;; Only three transitions:
    ;;   1. Escape (\) → skip next char (e.g. \", \\)
    ;;   2. Closing quote → return to normal mode
    ;;   3. Anything else → stay in string, advance
    [(string)
     (cond
       [(and st-obj (char-escape? ch st-obj))
        (values st #f (if (< pos1 len) (gap-next-char-pos gb pos1) len))]
       [(and st-obj (char-string-quote? ch st-obj))
        ;; Note: any string-quote char closes the string.  We don't require
        ;; the SAME quote char, matching font-lock's behavior.
        (values (struct-copy scan-state st [mode 'normal] [quote-ch #f])
                #f #f)]
       [else (values st #f #f)])]

    ;; ── block-comment ───────────────────────────────────────────────────
    ;; Inside a block comment (#| ... |#).
    ;; Tracks nesting depth for nestable comments.
    ;; End-str match → decrement block-depth.  If zero, return to normal.
    ;; Start-str match (if nestable) → increment block-depth.
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
        ;; Nested block comment opening inside another.
        ;; e.g. #| outer #| inner |# still outer |#
        (values (struct-copy scan-state st [block-depth (add1 block-depth)])
                #f (gap-skip-n gb p (string-length start-str)))]
       [else (values st #f #f)])]

    ;; ── here-string ─────────────────────────────────────────────────────
    ;; Inside a heredoc (#<<HERE ... HERE).
    ;; The delimiter must appear at BOL (beginning of line) to close.
    ;; After the delimiter, the immediate next char must be newline or EOF.
    [(here-string)
     (cond
       [(and delim (gap-at-bol? gb p) (gap-match-str-at gb p delim))
        (define delim-end (gap-skip-n gb p (string-length delim)))
        (cond
          [(>= delim-end len)
           ;; Delimiter at EOF — close the heredoc.
           (values (struct-copy scan-state st
                                [mode 'normal] [rule #f] [delim #f])
                   #f delim-end)]
          [(char=? (gap-char gb delim-end) #\newline)
           ;; Delimiter + newline — close.
           (values (struct-copy scan-state st
                                [mode 'normal] [rule #f] [delim #f])
                   #f (add1 delim-end))]
          [else
           ;; Delimiter matched but followed by non-newline char.
           ;; This is a false match (e.g. "HEREIS" starting with "HERE").
           ;; Stay in heredoc mode and advance normally.
           (values st #f #f)])]
       [else (values st #f #f)])]

    [else (values st #f #f)]))

;; ============================================================================
;; scan-chars! — core character scanner loop
;; ============================================================================
;;
;; Walks bytes [start, end) byte-by-byte, calling advance-char for each.
;; Accumulates:
;;   emits-rev   — (listof (cons face-name byte-pos)) in reverse order
;;   cps-rev     — (listof checkpoint?) in reverse order
;;
;; Records a checkpoint every `interval` bytes when the current mode is
;; 'normal.  We only checkpoint in normal mode because:
;;   1. String/comment interior checkpoints would need mode-specific state
;;      that is harder to compare for convergence.
;;   2. Edits rarely happen inside long strings (and if they do, the rescan
;;      from the prior normal checkpoint is correct, just slower).
;;
;; convergence-check? is called when all of:
;;   - current mode is 'normal
;;   - next mode is 'normal (i.e., no mode transition at this char)
;;   - no bracket face was emitted for this char
;; If it returns #t, scanning stops early.

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

       ;; Accumulate bracket-face emit
       (define new-emits
         (if emit-face
             (cons (cons emit-face p) emits-rev)
             emits-rev))

       ;; Checkpoint at interval boundary, normal mode only
       (define cur-mode  (scan-state-mode s))
       (define next-mode (scan-state-mode next-s))
       (define should-cp?
         (and (>= p (+ last-cp-pos interval))
              (> p start)
              (eq? cur-mode 'normal)))

       (define new-cps
         (if should-cp?
             (cons (checkpoint p s) cps-rev)
             cps-rev))
       (define new-last-ckpt
         (if should-cp? p last-cp-pos))

       ;; Convergence: only check in normal→normal, no face emitted.
       ;; Avoid checking on mode transitions or bracket positions because
       ;; those are the positions where state CHANGES — convergence is
       ;; only meaningful when nothing interesting happens.
       (cond
         [(and convergence-check?
               (not emit-face)
               (eq? cur-mode 'normal)
               (eq? next-mode 'normal)
               (convergence-check? (checkpoint p next-s)))
          (values s p new-cps new-emits)]
         [else
          (loop next-p next-s new-cps new-emits new-last-ckpt)])])))

;; ============================================================================
;; bracket-update! — incremental update after buffer change
;; ============================================================================
;;
;; Entry point called from main.rkt's event loop after every buffer mutation.
;; Extent comes from dirty-buffer's dirty-extent (merged change range).
;;
;; Algorithm (see module header for full description):
;;   1. Compute delta (new-len - old-len)
;;   2. Extend region ±15 lines via extend-change-region
;;   3. Partition checkpoints: valid (< scan-start) vs stale (≥ scan-start)
;;   4. Shift stale positions by delta
;;   5. Scan from last valid checkpoint to scan-end
;;   6. Try convergence against shifted stale checkpoints
;;   7. Clear bracket-face in [scan-start, scan-end), write new emits
;;   8. Update cache

(define (bracket-update! cache gb tp st extent)
  (if st
      (bracket-update-impl! cache gb tp st extent)
      ;; No syntax-table: just sync buf-len so future deltas are correct.
      (set-bracket-cache-buf-len! cache (gap-length gb))))

(define (bracket-update-impl! cache gb tp st extent)
  (match-define (cons ext-start ext-end) extent)
  (define new-len (gap-length gb))
  (define delta (- new-len (bracket-cache-buf-len cache)))
  (define interval (bracket-cache-interval cache))
  (define rules (syntax-table-multi-rules st))

  ;; Extend region with same heuristic as font-lock (±15 lines).
  ;; This is essential because font-lock's textprop-remove-key! also
  ;; covers this extended range — if bracket didn't extend the same way,
  ;; bracket-face properties previously written in the extended tail
  ;; would survive font-lock's removal but now be stale (wrong depth
  ;; after an edit that changes nesting).
  (match-define (cons scan-start scan-end)
    (extend-change-region gb ext-start ext-end))

  ;; Partition: valid = checkpoint position < scan-start (text before
  ;; this point is unchanged).  stale = everything else, shifted by delta.
  ;; The shift approximates the effect of the edit on downstream positions.
  ;; For single-edit commands this is exact.  For multi-edit batches it may
  ;; be slightly wrong, but convergence handles that (wrong shift → compare
  ;; fails → scan continues).  See limitation B in the module header.
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

  ;; Anchor: last valid checkpoint's state + position, or initial state at
  ;; scan-start if no valid checkpoints exist.
  (define start-st
    (if (pair? valid-cps)
        (checkpoint-state (last valid-cps))
        initial-scan-state))
  (define start-pos
    (if (pair? valid-cps)
        (checkpoint-pos (last valid-cps))
        scan-start))

  ;; Stale checkpoints as a mutable box.  convergence-check? drains this
  ;; list from the front as it matches or skips entries.
  (define stale-head (box stale-cps))

  (define (convergence-check? new-cp)
    (define scps (unbox stale-head))
    (cond
      [(null? scps) #f]   ; No more stale checkpoints to compare against.
      [else
       (define scp (car scps))
       (define spos (checkpoint-pos scp))
       (cond
         ;; Stale checkpoint is behind our current position.
         ;; This happens when the scanner jumped over it (e.g. line comment
         ;; skip jumped past a checkpoint position).  Drop it and try next.
         [(< spos (checkpoint-pos new-cp))
          (set-box! stale-head (cdr scps))
          (convergence-check? new-cp)]

         ;; Position matches AND state matches → CONVERGED!
         ;; The stale checkpoint (shifted to new coordinates) has exactly
         ;; the same state as our fresh scan.  Everything after this point
         ;; must produce identical results.
         [(and (= spos (checkpoint-pos new-cp))
               (equal? (checkpoint-state scp) (checkpoint-state new-cp)))
          #t]

         ;; Position matches but state DIFFERS → edit changed downstream
         ;; semantics.  Drop this stale entry and continue scanning.
         [(= spos (checkpoint-pos new-cp))
          (set-box! stale-head (cdr scps))
          #f]

         ;; spos > new-pos → haven't reached this stale entry yet.
         ;; Keep scanning forward.
         [else #f])]))

  ;; Core scan: from start-pos to scan-end, with convergence.
  (define-values (_final-st _final-pos new-cps-rev raw-emits)
    (scan-chars! gb start-pos scan-end start-st rules interval st
                 convergence-check?))

  ;; After scan completes, stale-head contains either:
  ;;   - '() if no convergence (all stale entries consumed)
  ;;   - remaining stale entries if converged (tail preserved)
  (define converged-tail (unbox stale-head))

  ;; Assemble final checkpoint list:
  ;; valid-checkpoints + newly-scanned-checkpoints + converged-tail
  (define new-checkpoints
    (append valid-cps (reverse new-cps-rev) converged-tail))

  ;; Apply bracket-face.  Two-step: remove old, write new.
  ;; Removal uses textprop-remove-key! which only removes 'bracket-face,
  ;; preserving font-lock's 'face properties in the same range.
  (textprop-remove-key! tp scan-start scan-end 'bracket-face)
  (for ([e (in-list (reverse raw-emits))])
    (match-define (cons face-name pos) e)
    (bracket-apply-emit! tp pos face-name))

  ;; Commit
  (set-bracket-cache-checkpoints! cache new-checkpoints)
  (set-bracket-cache-buf-len! cache new-len))

;; ============================================================================
;; bracket-rescan-all! — full buffer scan (initial setup)
;; ============================================================================
;;
;; Called once after buffer creation and syntax-table setup.
;; Wipes all bracket-face properties and checkpoints, scans from position 0.
;; Also used as a recovery mechanism if the cache gets corrupted
;; (though currently there's no corruption detection).

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

;; ============================================================================
;; bracket-state-at — query scan-state at a byte position
;; ============================================================================
;;
;; Finds the nearest checkpoint at or before `pos`, then scans forward
;; from that checkpoint to `pos`, returning the scan-state at pos
;; (state BEFORE consuming the character at pos).
;;
;; Returns #f if:
;;   - st is #f (no syntax-table — nothing to parse)
;;   - `pos` falls inside a skipped construct (line comment, heredoc body,
;;     escape sequence).  In this case the scanner jumped past `pos` in a
;;     single step, and there is no meaningful scan-state at that position.
;;
;; Used by bracket-find-match to determine if a position is in normal mode
;; before attempting sexp scanning.  Also useful for future features
;; (indentation, show-paren-mode).

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

;; scan-state-to-pos — scan from start-p/start-s forward to target.
;; Returns scan-state at target, or #f if the scanner jumped past target
;; (indicating target is inside a skipped construct).

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
       ;; If we jumped past target, we're inside something that was
       ;; skipped in a single step (line comment, heredoc, escape).
       (if (> next-p target) #f (loop next-p next-s))])))

;; ============================================================================
;; bracket-find-match — find matching bracket for show-paren-mode
;; ============================================================================
;;
;; Given a byte position of a bracket char, returns the byte position
;; of its matching bracket, or #f if:
;;   - no syntax-table
;;   - pos is out of range
;;   - scan-state at pos is not 'normal (e.g. inside string/comment)
;;   - char at pos is not a bracket
;;   - matching bracket not found (unbalanced)
;;
;; Delegates to motion.rkt's scan-sexp-forward/backward, which handle
;; proper paren matching with nesting, string skipping, and comment
;; skipping.  The bracket-cache is used only to verify that pos is in
;; normal mode — the actual matching is done by motion.rkt.

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
                   ;; Open bracket → scan forward for close.
                   ;; scan-sexp-forward returns pos AFTER close delim;
                   ;; we want the position OF the close delim.
                   (with-handlers ([exn:fail? (λ (_) #f)])
                     (define mp (scan-sexp-forward gb pos len st))
                     (and mp (< mp len)
                          (let ([mc (gap-prev-char-pos gb mp)])
                            (and (>= mc 0) mc))))]
                  [(or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
                   ;; Close bracket → scan backward for open.
                   ;; Start from position after this char.
                   (with-handlers ([exn:fail? (λ (_) #f)])
                     (scan-sexp-backward gb
                                         (add1 (gap-next-char-pos gb pos))
                                         st))]
                  [else #f]))))))
