#lang racket

;; kernel/data-debug/face-debug.rkt — Face-id range debug S-expressions

(require "../data/gap.rkt"
         "../data/face.rkt")

(provide
 face-debug-ranges      ;; gb → "(faces (F T id) ...)" — non-zero ranges
 face-debug-summary)    ;; gb → "(face-summary (non-zero N) (total M))"

(define (face-debug-ranges gb)
  (define len (gap-length gb))
  (define ranges
    (let scan ([pos 0] [acc '()])
      (cond [(>= pos len) (reverse acc)]
            [else
             (define fid (face-ref gb pos))
             (if (zero? fid)
                 (scan (add1 pos) acc)
                 (let find-end ([end (add1 pos)])
                   (if (and (< end len) (= (face-ref gb end) fid))
                       (find-end (add1 end))
                       (scan end (cons (list pos end fid) acc)))))])))
  (if (null? ranges)
      "(faces)"
      (format "(faces ~a)"
              (string-join
               (for/list ([r (in-list ranges)])
                 (match-define (list from to fid) r)
                 (format "(~a ~a ~a)" from to fid))
               " "))))

(define (face-debug-summary gb)
  (define len (gap-length gb))
  (define non-zero 0)
  (for ([i (in-range len)])
    (unless (zero? (face-ref gb i))
      (set! non-zero (add1 non-zero))))
  (format "(face-summary (non-zero ~a) (total ~a))" non-zero len))
