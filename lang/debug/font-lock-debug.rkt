#lang racket

;; lang/debug/font-lock-debug.rkt — Font-lock state debug S-expressions
;;
;; ============================================================================
;; Produces S-expression snapshots of font-locker state and scan results
;; for offline analysis of syntax highlighting.
;; ============================================================================

(require syntax-color/racket-lexer
         "../font-lock.rkt"
         "../../kernel/data/gap.rkt"
         "../../kernel/data/query.rkt"
         "../../kernel/data/face.rkt")

(provide
 ;; ── font-locker state ──
 font-locker-debug-summary    ;; fl → "(font-locker (comment N) (string N) ...)"

 ;; ── scan trace ──
 font-lock-token-trace        ;; fl gb start end → "(tokens (POS-POS lexeme TYPE fid) ...)"

 ;; ── face-id range dump ──
 font-lock-faces-debug        ;; gb start end → "(font-faces (POS-POS fid) ...)"

 ;; ── face coverage stats ──
 font-lock-stats-debug)       ;; gb start end → "(font-stats (total N) (colored M) ...)"

;; ============================================================
;; Helpers
;; ============================================================

(define (clamp-range gb start end)
  (define buflen (gap-length gb))
  (values (max 0 start) (min buflen end)))

;; ============================================================
;; font-locker-debug-summary
;; ============================================================

(define (font-locker-debug-summary fl)
  (unless (font-locker? fl)
    (raise-argument-error 'font-locker-debug-summary "font-locker?" fl))
  (define t->fid (font-locker-token->fid fl))
  (define kw-fid  (font-locker-keyword-fid fl))
  (define kw-count (set-count (font-locker-keywords fl)))
  (define type-parts
    (for/list ([(type fid) (in-hash t->fid)])
      (format "(~a ~a)" type fid)))
  (format "(font-locker (keyword ~a) (keywords ~a) ~a)"
          kw-fid kw-count (string-join type-parts " ")))

;; ============================================================
;; font-lock-token-trace — lex a range and show every token
;; ============================================================

(define (font-lock-token-trace fl gb start end)
  (unless (font-locker? fl)
    (raise-argument-error 'font-lock-token-trace "font-locker?" fl))
  (define-values (rstart rend) (clamp-range gb start end))
  (if (>= rstart rend)
      "(tokens)"
      (let* ([text    (gap-substring gb rstart rend)]
             [in      (open-input-string text)]
             [_       (port-count-lines! in)]
             [entries (collect-tokens fl gb in rstart)])
        (format-token-trace entries))))

(define (collect-tokens fl gb in base-bp)
  (let loop ([prev-bp base-bp] [prev-ce 0] [acc '()])
    (define-values (lexeme type paren char-start char-end)
      (racket-lexer in))
    (if (eq? type 'eof)
        (reverse acc)
        (let* ([cs0      (sub1 char-start)]
               [skip     (max 0 (- cs0 prev-ce))]
               [bp-start (gap-skip-n gb prev-bp skip)]
               [bp-end   (gap-skip-n gb bp-start (- char-end char-start))]
               [fid      (token-face-id fl type lexeme)]
               [lex      (if (> (string-length lexeme) 40)
                             (format "~a..." (substring lexeme 0 40))
                             lexeme)]
               [entry    (list bp-start char-end type lex fid)])
          (loop bp-end char-end (cons entry acc))))))

(define (format-token-trace entries)
  (if (null? entries)
      "(tokens)"
      (format "(tokens ~a)"
              (string-join
               (for/list ([e (in-list entries)])
                 (match-define (list bp ce type lex fid) e)
                 (format "(~a-~a ~s ~s ~a)" bp ce lex type fid))
               " "))))

;; ============================================================
;; font-lock-faces-debug — face-id range dump
;; ============================================================

(define (font-lock-faces-debug gb start end)
  (define-values (rstart rend) (clamp-range gb start end))
  (if (>= rstart rend)
      "(font-faces)"
      (let ([ranges (collect-face-ranges gb rstart rend)])
        (if (null? ranges)
            "(font-faces)"
            (format "(font-faces ~a)"
                    (string-join
                     (for/list ([r (in-list ranges)])
                       (match-define (list from to fid) r)
                       (format "(~a-~a ~a)" from to fid))
                     " "))))))

(define (collect-face-ranges gb start end)
  (let loop ([pos start] [acc '()])
    (cond [(>= pos end) (reverse acc)]
          [else
           (define fid (face-ref gb pos))
           (if (zero? fid)
               (loop (add1 pos) acc)
               (let find-end ([ep (add1 pos)])
                 (if (and (< ep end) (= (face-ref gb ep) fid))
                     (find-end (add1 ep))
                     (loop ep (cons (list pos ep fid) acc)))))])))

;; ============================================================
;; font-lock-stats-debug — coverage statistics
;; ============================================================

(define (font-lock-stats-debug gb start end)
  (define-values (rstart rend) (clamp-range gb start end))
  (if (>= rstart rend)
      "(font-stats (total 0))"
      (let* ([total  (- rend rstart)]
             [counts (make-hasheq)]
             [colored 0])
        (for ([pos (in-range rstart rend)])
          (define fid (face-ref gb pos))
          (unless (zero? fid)
            (set! colored (add1 colored))
            (hash-update! counts fid (λ (c) (add1 c)) 0)))
        (define parts
          (for/list ([(fid cnt) (in-hash counts)])
            (format "(fid~a ~a)" fid cnt)))
        (format "(font-stats (total ~a) (colored ~a) (uncolored ~a) ~a)"
                total colored (- total colored)
                (string-join parts " ")))))
