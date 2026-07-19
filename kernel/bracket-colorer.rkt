#lang racket

;; kernel/bracket-colorer.rkt — Bracket depth coloring engine
;;
;; ============================================================================
;; Compositional, checkpoint-based bracket depth coloring.
;;
;; ── Architecture ──────────────────────────────────────────────────────────
;;
;;   Pure computation (reads gap buffer, returns data, never mutates):
;;
;;     scan-state         — immutable snapshot (depth, mode, stack, ...)
;;     checkpoint         — (pos . scan-state) convergence anchor
;;     advance-char       — scan-state × char × syntax-table → next-state × emit?
;;     scan-chars         — folds advance-char over [start, end)
;;
;;   Imperative application (writes face-ids to gap buffer's face array):
;;
;;     bracket-colorer            — mutable container (checkpoints, buf-len, fids)
;;     bracket-colorer-rescan-all!  — full scan + apply
;;     bracket-colorer-update!      — incremental update after edit
;;
;; ── Face-id convention ────────────────────────────────────────────────────
;;
;;   Face-ids are resolved ONCE at construction time from the face-cache,
;;   stored as bare u8 integers in the bracket-colorer, and written directly
;;   to the gap buffer's face array via face-set! (kernel/data/face.rkt).
;;
;;   6 depth colors (depth % 6 cycling) + 1 mismatch face.
;;   Non-bracket bytes keep their existing face-id (0 = default, or future
;;   font-lock face-ids).
;;
;;   When font-lock arrives, it runs AFTER bracket-colorer-update!, writing
;;   font-lock face-ids over bracket face-ids where it has its own data.
;;   The write ordering provides the correct priority: font-lock > bracket.
;;
;; ── Checkpoint convergence ────────────────────────────────────────────────
;;
;;   Every `interval` (default 1024) bytes in normal mode, a checkpoint is
;;   recorded.  On edit, we restart from the last valid checkpoint before
;;   the edit, scan forward, and compare against shifted stale checkpoints.
;;   When scan-state matches → converged — stop scanning.
;;
;;   Most edits touch a few lines → O(Δ).  Structural edits (insert/delete
;;   paren) fail convergence at each checkpoint → O(file), which is the
;;   correct lower bound for those edits.
;;
;; ── Supported modes ───────────────────────────────────────────────────────
;;
;;   normal          — bracket detection, entry to string/comment/block
;;   string          — "..." with \ escape skipping
;;   line-comment    — ; ... \n → back to normal
;;   block-comment   — #| ... |# with nesting support
;;
;;   Heredoc (#<<HERE) is not supported in this version.
;;
;; ── Dependencies ──────────────────────────────────────────────────────────
;;
;;   kernel/data/gap.rkt       — gap-length, gap-buffer-*
;;   kernel/data/query.rkt     — gap-char, gap-next-char-pos, gap-scan-byte, ...
;;   kernel/data/face.rkt      — face-set!, face-fill!
;;   kernel/data/syntax.rkt    — syntax-table, multi-char-rule, char-* predicates
;;   display/face.rkt          — define-face!, face-id-for-name, make-face-attrs
;;
;; ============================================================================

(require "data/gap.rkt"
         "data/query.rkt"
         "data/face.rkt"
         "data/syntax.rkt"
         "../display/face.rkt")

(provide
 ;; ── data structures ──
 scan-state? checkpoint? bracket-colorer?
 bracket-colorer-depth-fids bracket-colorer-mismatch-fid
 bracket-colorer-checkpoints bracket-colorer-buf-len
 make-bracket-colorer

 ;; ── pure computation ──
 initial-scan-state
 advance-char
 scan-chars

 ;; ── imperative application ──
 bracket-colorer-rescan-all!
 bracket-colorer-update!

 ;; ── face registration ──
 bracket-register-faces!)

;; ============================================================
;; Data structures
;; ============================================================

;; scan-state — immutable, #:transparent for equal? convergence checking.
;;
;;   depth        — unmatched open brackets before this position
;;   mode         — 'normal | 'string | 'line-comment | 'block-comment
;;   quote-ch     — char that opened the string (for " matching)
;;   rule         — active multi-char-rule for block-comment
;;   block-depth  — nesting count inside nestable block-comments
;;   stack        — unmatched opens, innermost at head: '(#\{ #\[ #\()
(struct scan-state
  (depth mode quote-ch rule block-depth stack)
  #:transparent)

(define initial-scan-state
  (scan-state 0 'normal #f #f 0 '()))

;; checkpoint — position × state snapshot for convergence.
(struct checkpoint (pos state) #:transparent)

;; bracket-colorer — mutable container for the coloring engine.
;;
;;   checkpoints  — ascending list of checkpoint by position.
;;   buf-len      — gap-length at last scan completion.
;;   interval     — bytes between checkpoints (default 1024).
;;   depth-fids   — vector of 6 face-ids for depth 0..5.
;;   mismatch-fid — face-id for mismatched brackets.
(struct bracket-colorer
  ([checkpoints #:mutable]
   [buf-len     #:mutable]
   interval
   depth-fids
   mismatch-fid)
  #:transparent)

;; ============================================================
;; Face registration — called once at startup from main.rkt
;; ============================================================

(define bracket-depth-faces
  (vector 'bracket-depth-0-face
          'bracket-depth-1-face
          'bracket-depth-2-face
          'bracket-depth-3-face
          'bracket-depth-4-face
          'bracket-depth-5-face))

(define bracket-mismatch-face 'bracket-mismatch-face)

(define (bracket-register-faces!)
  (define colors
    (list (list 255 180 0)   ; gold
          (list 180 120 255) ; purple
          (list 80  200 255) ; cyan
          (list 255 100 100) ; red
          (list 100 255 100) ; green
          (list 255 200 80))); orange
  (for ([(c i) (in-indexed (in-list colors))])
    (define-face! (vector-ref bracket-depth-faces i)
                 (make-face-attrs 'foreground c)))
  (define-face! bracket-mismatch-face
                (make-face-attrs 'foreground (list 255 255 255)
                                 'background (list 180 0 0))))

(define (bracket-face-for-depth depth fids)
  (vector-ref fids (modulo depth (vector-length fids))))

;; ============================================================
;; Constructor — resolves face-ids from face-cache once
;; ============================================================

(define (make-bracket-colorer fc [interval 1024])
  (unless fc
    (raise-argument-error 'make-bracket-colorer "face-cache?" fc))
  (bracket-colorer
   '() 0 interval
   (for/vector ([name (in-vector bracket-depth-faces)])
     (face-id-for-name name fc))
   (face-id-for-name bracket-mismatch-face fc)))

;; ============================================================
;; Bracket helpers
;; ============================================================

(define (open-bracket? ch)
  (or (char=? ch #\() (char=? ch #\[) (char=? ch #\{)))

(define (close-bracket? ch)
  (or (char=? ch #\)) (char=? ch #\]) (char=? ch #\})))

(define (matching-open ch)
  (case ch [(#\)) #\(] [(#\]) #\[] [(#\}) #\{] [else #\(]))

;; ============================================================
;; advance-char — pure state transition
;; ============================================================
;;
;; Signature:
;;   gap-buffer? byte-pos byte-pos char?
;;   scan-state? (listof multi-char-rule?) syntax-table?
;;   (vectorof face-id?) face-id?
;;   → (values scan-state? (or/c #f face-id?) (or/c #f byte-pos?))
;;
;; Returns:
;;   next-state     — scan-state after consuming this character
;;   emit-face?     — #f or a face-id to emit at byte-pos (bracket found)
;;   skip-pos?      — #f (advance normally) or byte-pos to jump to
;;
;; p    = current byte position
;; pos1 = next byte position (gap-next-char-pos gb p)

(define (advance-char gb p pos1 ch st rules st-obj fids mm-fid)
  (match-define (scan-state depth mode quote-ch rule block-depth stack) st)
  (define len (gap-length gb))

  (case mode
    ;; ── normal ──────────────────────────────────────────────────────────
    [(normal)
     (cond
       ;; Multi-char rule start (block-comment #| ... |#)
       [(for/or ([r (in-list rules)])
          (and (equal? (multi-char-rule-tag r) 'block-comment)
               (gap-match-str-at gb p (multi-char-rule-start-str r))
               r))
        => (λ (r)
            (define slen (string-length (multi-char-rule-start-str r)))
            (values (struct-copy scan-state st
                                 [mode 'block-comment]
                                 [rule r]
                                 [block-depth 1])
                    #f (gap-skip-n gb p slen)))]

       ;; Open bracket — push, emit depth-colored face.
       [(or (char=? ch #\() (char=? ch #\[) (char=? ch #\{))
        (values (struct-copy scan-state st
                             [depth (add1 depth)]
                             [stack (cons ch stack)])
                (bracket-face-for-depth depth fids)
                #f)]

       ;; Close bracket — match against stack top.
       [(or (char=? ch #\)) (char=? ch #\]) (char=? ch #\}))
        (cond
          [(and (pair? stack) (char=? (car stack) (matching-open ch)))
           (define new-depth (sub1 depth))
           (values (struct-copy scan-state st
                                [depth new-depth]
                                [stack (cdr stack)])
                   (bracket-face-for-depth new-depth fids)
                   #f)]
          [else (values st mm-fid #f)])]

       ;; String quote (") — enter string mode.
       [(and st-obj (char-string-quote? ch st-obj))
        (values (struct-copy scan-state st [mode 'string] [quote-ch ch])
                #f #f)]

       ;; Line comment (;) — jump to end of line.
       [(and st-obj (char-comment-start? ch st-obj))
        (define nl (gap-scan-byte gb p 'forward (λ (b) (= b #x0A))))
        (values st #f (if (< nl len) (add1 nl) len))]

       ;; Escape (\) — skip next character (handles #\( etc.).
       [(and st-obj (char-escape? ch st-obj))
        (values st #f (if (< pos1 len) (gap-next-char-pos gb pos1) len))]

       ;; Default — word, whitespace, punctuation, expression-prefix.
       [else (values st #f #f)])]

    ;; ── string ──────────────────────────────────────────────────────────
    [(string)
     (cond
       [(and st-obj (char-escape? ch st-obj))
        (values st #f (if (< pos1 len) (gap-next-char-pos gb pos1) len))]
       [(and st-obj (char-string-quote? ch st-obj))
        (values (struct-copy scan-state st [mode 'normal] [quote-ch #f])
                #f #f)]
       [else (values st #f #f)])]

    ;; ── block-comment ───────────────────────────────────────────────────
    [(block-comment)
     (define end-str   (multi-char-rule-end-str rule))
     (define start-str (multi-char-rule-start-str rule))
     (cond
       [(gap-match-str-at gb p end-str)
        (define new-depth (sub1 block-depth))
        (define end-len (string-length end-str))
        (if (zero? new-depth)
            (values (struct-copy scan-state st
                                 [mode 'normal] [rule #f] [block-depth 0])
                    #f (gap-skip-n gb p end-len))
            (values (struct-copy scan-state st [block-depth new-depth])
                    #f (gap-skip-n gb p end-len)))]
       [(and (multi-char-rule-nestable? rule)
             (gap-match-str-at gb p start-str))
        (values (struct-copy scan-state st [block-depth (add1 block-depth)])
                #f (gap-skip-n gb p (string-length start-str)))]
       [else (values st #f #f)])]

    [else (values st #f #f)]))

;; ============================================================
;; scan-chars — pure scan loop with checkpoint convergence
;; ============================================================
;;
;; Walks [start, end) byte-by-byte, calling advance-char at each step.
;; Maintains a parallel position stack (not in scan-state, so equal?
;; convergence is preserved) to track unclosed open brackets.
;;
;; Returns (values final-state final-pos checkpoints emits unclosed-opens)
;;   checkpoints    — (listof checkpoint?) forward order
;;   emits          — (listof (cons byte-pos face-id)) forward order
;;   unclosed-opens — (listof byte-pos) positions of still-open brackets

(define (scan-chars gb start end st rules interval st-obj fids mm-fid
                    #:convergence? [convergence? (λ (_cp) #f)])
  (define len (gap-length gb))
  (define real-end (min end len))

  (let loop ([p start]
             [s st]
             [pos-stack '()]   ; parallel to s.stack — unmatched open positions
             [cps-rev '()]
             [emits-rev '()]
             [last-cp-pos start])
    (cond
      [(>= p real-end)
       (values s p (reverse cps-rev) (reverse emits-rev) (reverse pos-stack))]

      [else
       (define ch   (gap-char gb p))
       (define pos1 (gap-next-char-pos gb p))

       (define-values (next-s emit-fid skip-pos)
         (advance-char gb p pos1 ch s rules st-obj fids mm-fid))

       (define next-p (or skip-pos pos1))

       ;; Track open-bracket positions parallel to scan-state stack.
       ;; Open → push p.  Close-match → pop.  Mismatch-close → keep.
       (define next-pos-stack
         (cond
           [(and emit-fid (open-bracket? ch))
            (cons p pos-stack)]
           [(and emit-fid (close-bracket? ch) (not (= emit-fid mm-fid)))
            (if (pair? pos-stack) (cdr pos-stack) pos-stack)]
           [else pos-stack]))

       ;; Accumulate face emission
       (define new-emits
         (if emit-fid (cons (cons p emit-fid) emits-rev) emits-rev))

       ;; Checkpoint: at interval boundary, normal mode only
       (define cur-mode (scan-state-mode s))
       (define should-cp?
         (and (>= p (+ last-cp-pos interval))
              (> p start)
              (eq? cur-mode 'normal)))

       (define new-cps
         (if should-cp? (cons (checkpoint p s) cps-rev) cps-rev))
       (define new-last-ckpt (if should-cp? p last-cp-pos))

       ;; Convergence: only in normal→normal, no face emitted
       (define next-mode (scan-state-mode next-s))
       (cond
         [(and (not emit-fid)
               (eq? cur-mode 'normal)
               (eq? next-mode 'normal)
               (convergence? (checkpoint p next-s)))
          (values s p (reverse new-cps) (reverse new-emits) (reverse next-pos-stack))]
         [else
          (loop next-p next-s next-pos-stack new-cps new-emits new-last-ckpt)])])))

;; ============================================================
;; apply-emits! — write face-ids to gap buffer's face array
;; ============================================================

(define (apply-emits! gb emits)
  (for ([e (in-list emits)])
    (match-define (cons pos fid) e)
    (face-set! gb pos fid)))

;; ============================================================
;; extend-scan-range — ±15 lines from edit position
;; ============================================================
;;
;; Pure: gap-buffer × edit-start × edit-len → (values start end)

(define (extend-scan-range gb edit-start edit-len)
  (define buflen (gap-length gb))
  (define lines 15)
  (define edit-end (min buflen (+ edit-start edit-len)))

  (define sol
    (let loop ([pos edit-start] [remaining lines])
      (if (or (zero? pos) (zero? remaining))
          pos
          (let ([prev-nl (gap-scan-byte gb (sub1 pos) 'backward
                                        (λ (b) (= b #x0A)))])
            (if (>= prev-nl 0)
                (loop (add1 prev-nl) (sub1 remaining))
                0)))))

  (define eol
    (let loop ([pos edit-end] [remaining lines])
      (define nl (gap-scan-byte gb pos 'forward (λ (b) (= b #x0A))))
      (if (or (>= nl buflen) (zero? remaining))
          (min nl buflen)
          (loop (add1 nl) (sub1 remaining)))))

  (values sol eol))

;; ============================================================
;; bracket-colorer-rescan-all! — full buffer scan
;; ============================================================

(define (bracket-colorer-rescan-all! bkt gb st)
  (unless (bracket-colorer? bkt)
    (raise-argument-error 'bracket-colorer-rescan-all! "bracket-colorer?" bkt))

  (unless st
    (set-bracket-colorer-checkpoints! bkt '())
    (set-bracket-colorer-buf-len! bkt (gap-length gb)))

  ;; Reset state
  (set-bracket-colorer-checkpoints! bkt '())
  (set-bracket-colorer-buf-len! bkt 0)

  (define buflen (gap-length gb))
  (when (positive? buflen)
    (define rules    (syntax-table-multi-rules st))
    (define interval (bracket-colorer-interval bkt))
    (define fids     (bracket-colorer-depth-fids bkt))
    (define mm-fid   (bracket-colorer-mismatch-fid bkt))

    ;; Clear faces, then scan and apply
    (face-fill! gb 0 buflen 0)

    (define-values (_st _pos cps emits unclosed)
      (scan-chars gb 0 buflen initial-scan-state
                  rules interval st fids mm-fid))

    (set-bracket-colorer-checkpoints! bkt cps)
    (set-bracket-colorer-buf-len! bkt buflen)

    (apply-emits! gb emits)
    ;; Fix unclosed open brackets → mismatch face
    (for ([pos (in-list unclosed)])
      (face-set! gb pos mm-fid))))

;; ============================================================
;; bracket-colorer-update! — incremental update after edit
;; ============================================================
;;
;; Called after dirty-commit!, before dirty-clear!.
;; edit-start: byte position where edit occurred.
;; delta: net byte change (> 0 for insert, < 0 for delete).

(define (bracket-colorer-update! bkt gb st edit-start delta)
  (unless (bracket-colorer? bkt)
    (raise-argument-error 'bracket-colorer-update! "bracket-colorer?" bkt))

  (unless st
    (set-bracket-colorer-buf-len! bkt (gap-length gb)))

  (define new-len  (gap-length gb))
  (define old-len  (bracket-colorer-buf-len bkt))
  (define interval (bracket-colorer-interval bkt))
  (define rules    (syntax-table-multi-rules st))
  (define fids     (bracket-colorer-depth-fids bkt))
  (define mm-fid   (bracket-colorer-mismatch-fid bkt))

  ;; Extend scan range: ±15 lines from edit
  (define edit-len (max 1 (abs delta)))
  (define-values (scan-start scan-end)
    (extend-scan-range gb edit-start edit-len))

  ;; ── Partition checkpoints ──
  ;; valid: pos < scan-start (untouched by edit)
  ;; stale: pos ≥ scan-start, shifted by delta
  (define old-cps (bracket-colorer-checkpoints bkt))
  (define-values (valid-cps stale-cps)
    (let part ([cps old-cps] [v '()] [s '()])
      (cond
        [(null? cps) (values (reverse v) (reverse s))]
        [else
         (define cp (car cps))
         (if (< (checkpoint-pos cp) scan-start)
             (part (cdr cps) (cons cp v) s)
             (part (cdr cps) v
                   (cons (struct-copy checkpoint cp
                            [pos (+ (checkpoint-pos cp) delta)])
                         s)))])))

  ;; ── Anchor ──
  (define start-st
    (if (pair? valid-cps)
        (checkpoint-state (last valid-cps))
        initial-scan-state))
  (define start-pos
    (if (pair? valid-cps)
        (checkpoint-pos (last valid-cps))
        scan-start))

  ;; ── Convergence callback (closes over stale-head box) ──
  (define stale-head (box stale-cps))

  (define (convergence? new-cp)
    (let loop ()
      (define scps (unbox stale-head))
      (cond
        [(null? scps) #f]
        [else
         (define scp (car scps))
         (define spos (checkpoint-pos scp))
         (cond
           ;; Stale checkpoint behind us — drop and try next
           [(< spos (checkpoint-pos new-cp))
            (set-box! stale-head (cdr scps))
            (loop)]
           ;; Position AND state match → CONVERGED
           [(and (= spos (checkpoint-pos new-cp))
                 (equal? (checkpoint-state scp) (checkpoint-state new-cp)))
            #t]
           ;; Position matches, state differs — drop and continue
           [(= spos (checkpoint-pos new-cp))
            (set-box! stale-head (cdr scps))
            #f]
           ;; spos > new-pos — not reached yet, keep scanning
           [else #f])])))

  ;; ── Core scan ──
  (define-values (_final-st _final-pos new-cps emits unclosed)
    (scan-chars gb start-pos scan-end start-st
                rules interval st fids mm-fid
                #:convergence? convergence?))

  ;; ── Assemble checkpoints ──
  (define all-checkpoints
    (append valid-cps new-cps (unbox stale-head)))

  ;; ── Apply faces ──
  ;; Clear the scan range first (future font-lock will re-apply its faces
  ;; AFTER bracket coloring, overwriting brackets where it has its own data).
  (face-fill! gb start-pos scan-end 0)
  (apply-emits! gb emits)
  ;; Fix unclosed open brackets → mismatch face
  (for ([pos (in-list unclosed)])
    (face-set! gb pos mm-fid))

  ;; ── Commit ──
  (set-bracket-colorer-checkpoints! bkt all-checkpoints)
  (set-bracket-colorer-buf-len! bkt new-len))
