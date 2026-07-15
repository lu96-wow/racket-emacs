#lang racket
;; test/protocol/keymap-test.rkt
(require rackunit
         "../../protocol/keymap.rkt"
         "../../kernel/key-event/key-event.rkt")

(test-case "make-keymap is empty"
  (define km (make-keymap))
  (check-equal? (hash-count (keymap-bindings km)) 0)
  (check-false (keymap-parent km)))

(test-case "define-key and lookup-key single binding"
  (define km (make-keymap))
  (define ke (key-event #\a #f #f #f #f))
  (define-key km (list ke) 'self-insert)
  (check-equal? (lookup-key km (list ke)) 'self-insert))

(test-case "lookup-key returns #f for unknown"
  (define km (make-keymap))
  (check-false (lookup-key km (list (key-event #\x #f #f #f #f)))))

(test-case "define-key prefix sequence"
  (define km (make-keymap))
  (define-key km (list (key-event #\a #t #f #f #f)    ;; C-a
                        (key-event #\b #t #f #f #f))   ;; C-b
              'two-key-cmd)
  ;; Full sequence matches
  (check-equal? (lookup-key km (list (key-event #\a #t #f #f #f)
                                      (key-event #\b #t #f #f #f)))
                'two-key-cmd)
  ;; Prefix returns keymap
  (define sub (lookup-key km (list (key-event #\a #t #f #f #f))))
  (check-true (keymap? sub)))

(test-case "key normalization: Ctrl-letter"
  (define km (make-keymap))
  ;; Define with C-a (ctrl flag)
  (define-key km (list (key-event #\a #t #f #f #f)) 'ctrl-a-cmd)
  ;; Lookup with C-a
  (check-equal? (lookup-key km (list (key-event #\a #t #f #f #f)))
                'ctrl-a-cmd))

(test-case "parent keymap fallback"
  (define parent (make-keymap))
  (define-key parent (list (key-event #\a #f #f #f #f)) 'from-parent)
  (define child (make-keymap parent))
  (define-key child (list (key-event #\b #f #f #f #f)) 'from-child)
  ;; Child's own binding
  (check-equal? (lookup-key child (list (key-event #\b #f #f #f #f)))
                'from-child)
  ;; Fallback to parent
  (check-equal? (lookup-key child (list (key-event #\a #f #f #f #f)))
                'from-parent)
  ;; Unknown in both
  (check-false (lookup-key child (list (key-event #\c #f #f #f #f)))))

(test-case "child overrides parent"
  (define parent (make-keymap))
  (define-key parent (list (key-event #\a #f #f #f #f)) 'parent-val)
  (define child (make-keymap parent))
  (define-key child (list (key-event #\a #f #f #f #f)) 'child-val)
  (check-equal? (lookup-key child (list (key-event #\a #f #f #f #f)))
                'child-val))

(test-case "named key binding"
  (define km (make-keymap))
  (define-key km (list (key-event #f #f #f #f 'up)) 'up-cmd)
  (check-equal? (lookup-key km (list (key-event #f #f #f #f 'up)))
                'up-cmd))

(test-case "key-sequence->description"
  (define ks (list (key-event #\a #t #f #f #f)
                   (key-event #\x #f #f #f #f)))
  (check-equal? (key-sequence->description ks) "C-a x"))
