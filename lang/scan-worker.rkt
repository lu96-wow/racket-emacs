#lang racket

;; lang/scan-worker.rkt — Asynchronous background scanner
;;
;; Spawns scanner threads with immutable buffer snapshots.
;; Results are merged back into text-props on the main thread via
;; non-blocking poll.  Version tracking discards stale results
;; when the user edits faster than scanning completes.
;;
;; Architecture:
;;   scan-worker-schedule!  — snapshot buffer, spawn thread
;;   scan-worker-collect!   — try-get result from channel, apply faces
;;
;; The pure scanner functions (syntax-scan/list, keyword-scan/list,
;; bracket-scan/list) operate on a snapshot gap-buffer that is never
;; mutated → thread-safe without locks.
;;
;; Dependencies: font-lock (scanners), bracket-cache (bracket-scan/list),
;;   kernel/data (gap, query, textprop, syntax).

(require "font-lock.rkt"
         "bracket-cache.rkt"
         "../kernel/data/gap.rkt"
         "../kernel/data/query.rkt"
         "../kernel/data/textprop.rkt"
         "../kernel/data/syntax.rkt")

(provide
 scan-worker? make-scan-worker
 scan-worker-schedule!
 scan-worker-collect!
 scan-worker-pending?)

;; ============================================================
;; Data
;; ============================================================

(struct scan-worker
  ([result-ch #:mutable]   ; channel — carries (cons version face-list)
   [version   #:mutable]   ; integer — incremented each schedule
   [pending   #:mutable])  ; boolean — #t if a scan thread is in-flight
  #:transparent)

(define (make-scan-worker)
  (scan-worker (make-channel) 0 #f))

;; ============================================================
;; scan-worker-schedule! — snapshot + spawn thread
;; ============================================================

(define (scan-worker-schedule! w gb st config extent)
  ;; Snapshots the full buffer text into an independent gap-buffer,
  ;; spawns a thread running pure scanners on it.  Results carry a
  ;; version tag — if the buffer changes before results arrive,
  ;; collect! silently discards them.
  (match-define (cons ext-start ext-end) extent)
  (set-scan-worker-version! w (add1 (scan-worker-version w)))
  (define my-ver (scan-worker-version w))

  ;; Snapshot full buffer as an independent, immutable-for-reading gb.
  (define full-text (gap-substring gb 0 (gap-length gb)))
  (define snapshot (make-gap-buffer full-text))

  ;; Compute the extended scan region (same heuristic as sync path).
  (match-define (cons scan-start scan-end)
    (extend-change-region gb ext-start ext-end))

  ;; Capture config fields for the thread (struct access is cheap,
  ;; but safer to extract before the thread).
  (define st-table  (and config (syntax-config-syntax-table config)))
  (define keywords  (and config (syntax-config-keywords config)))
  (define case-fold (and config (syntax-config-case-fold? config)))

  (define ch (scan-worker-result-ch w))

  (set-scan-worker-pending! w #t)
  (thread
   (λ ()
     ;; font-lock faces
     (define fl-faces
       (if st-table
           (append
            (syntax-scan/list snapshot st-table scan-start scan-end)
            (keyword-scan/list snapshot keywords scan-start scan-end case-fold))
           '()))
     ;; bracket faces
     (define bk-faces
       (if st-table
           (bracket-scan/list snapshot st-table scan-start scan-end)
           '()))
     ;; Send result: version + face lists + scan region.
     (channel-put ch (cons my-ver
                           (cons fl-faces
                                 (cons bk-faces
                                       (cons scan-start scan-end))))))
    ))

;; ============================================================
;; scan-worker-collect! — non-blocking result collection
;; ============================================================

(define (scan-worker-collect! w tp)
  ;; Poll the channel (non-blocking).  If a result is ready AND its
  ;; version matches the current version, apply faces to text-props.
  ;; If version mismatches, discard (stale — a newer edit has been
  ;; scheduled).  Returns #t if faces were applied, #f otherwise.
  (define ch (scan-worker-result-ch w))
  (define res (channel-try-get ch))
  (cond
    [(not res) #f]
    [else
     (match-define (cons ver (cons fl-faces (cons bk-faces region))) res)
     (match-define (cons scan-start scan-end) region)
     (cond
       [(= ver (scan-worker-version w))
        ;; Version match — clear old faces in scan region, then write new.
        (textprop-remove-key! tp scan-start scan-end 'face)
        (textprop-remove-key! tp scan-start scan-end 'bracket-face)
        (for ([f (in-list fl-faces)])
          (match-define (list s e face-name) f)
          (textprop-put! tp s e 'face face-name))
        (for ([f (in-list bk-faces)])
          (match-define (cons face-name pos) f)
          (define pos2 (add1 pos))
          (when (< pos pos2)
            (textprop-put! tp pos pos2 'bracket-face face-name)))
        (set-scan-worker-pending! w #f)
        #t]
       [else
        ;; Stale — a newer scan has been scheduled.  Discard.
        (set-scan-worker-pending! w #f)
        #f])]))

(define (scan-worker-pending? w)
  (scan-worker-pending w))
