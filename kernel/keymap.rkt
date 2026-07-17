#lang racket

;; kernel/keymap.rkt - Flat keymap: key to command-or-keymap, with merge composition
;;
;; A keymap is a single hash table. No parent chain, no inheritance.
;; Multiple maps compose by merging into one effective map: later
;; entries overwrite earlier ones. Lookup is always O(1) single-table.
;;
;; Key sequences (C-x C-f) produce nested keymaps automatically.
;; The event loop maintains prefix state; this module only provides
;; data + lookup.
;;
;; Dependencies: kernel/key-event.rkt

(require "key-event.rkt")

(provide
 ;; types
 keymap? make-keymap
 command? command command-name command-fn

 ;; single-table ops
 keymap-set! keymap-ref keymap-remove!
 keymap-empty? keymap-keys

 ;; composition
 keymap-merge!

 ;; key normalisation
 key-event->key

 ;; sequence binding
 keymap-bind-key!

 ;; classification
 self-insert-key-event?)

;; ============================================================
;; Command
;; ============================================================

(struct command (name fn) #:transparent)

;; ============================================================
;; Keymap
;; ============================================================

(struct keymap (table) #:transparent
  #:methods gen:custom-write
  [(define (write-proc km port mode)
     (fprintf port "#<keymap ~a bindings>" (hash-count (keymap-table km))))])

(define (make-keymap)
  (keymap (make-hash)))

;; ============================================================
;; Single-table ops
;; ============================================================

(define (keymap-set! km key value)
  (hash-set! (keymap-table km) key value))

(define (keymap-ref km key)
  (hash-ref (keymap-table km) key (lambda () #f)))

(define (keymap-remove! km key)
  (hash-remove! (keymap-table km) key))

(define (keymap-empty? km)
  (= (hash-count (keymap-table km)) 0))

(define (keymap-keys km)
  (hash-keys (keymap-table km)))

;; ============================================================
;; Composition
;; ============================================================

(define (keymap-merge! dst src)
  (for ([(k v) (in-hash (keymap-table src))])
    (hash-set! (keymap-table dst) k v)))

;; ============================================================
;; key-event->key
;; ============================================================

(define (key-event->key ke)
  ;; Returns a hashable key: char for self-insert, symbol for named/modified.
  (define ch   (key-event-char ke))
  (define ctrl (key-event-ctrl? ke))
  (define meta (key-event-meta? ke))
  (define sym  (key-event-symbol ke))

  (cond
    ;; Named keys: up -> up, M-up -> M-up, C-up -> C-up
    [sym
     (define parts
       (append (if meta '("M") '())
               (if ctrl '("C") '())
               (list (symbol->string sym))))
     (string->symbol (string-join parts "-"))]

    ;; C-char: C-a, C-SPC, ...
    [(and ch ctrl)
     (if (char=? ch #\space)
         'C-SPC
         (string->symbol
          (format "C-~a" (char-downcase ch))))]

    ;; M-char: M-x, ...
    [(and ch meta)
     (string->symbol
      (format "M-~a" (char-downcase ch)))]

    ;; Plain printable
    [ch ch]

    [else #f]))

;; ============================================================
;; Sequence binding
;; ============================================================

(define (keymap-bind-key! km keys cmd)
  ;; keys: (listof key?)  cmd: command?
  ;; Signals error on conflicts:
  ;;   - last key already a prefix (keymap value)
  ;;   - intermediate key already bound to a command
  (when (null? keys)
    (error 'keymap-bind-key! "empty key sequence"))
  (let loop ((km km) (keys keys))
    (define k (car keys))
    (define existing (keymap-ref km k))
    (if (null? (cdr keys))
        ;; Final key -- must not already be a prefix
        (begin
          (when (keymap? existing)
            (error 'keymap-bind-key!
                   "key ~e is already a prefix key" k))
          (keymap-set! km k cmd))
        ;; Intermediate key -- must not already be a command
        (begin
          (when (command? existing)
            (error 'keymap-bind-key!
                   "key ~e is already bound to a command; cannot make it a prefix"
                   k))
          (let ((next (if (keymap? existing) existing (make-keymap))))
            (keymap-set! km k next)
            (loop next (cdr keys)))))))

;; ============================================================
;; Self-insert classification
;; ============================================================

(define (self-insert-key-event? ke)
  (and (key-event? ke)
       (let ((ch (key-event-char ke)))
         (and ch
              (not (key-event-ctrl? ke))
              (not (key-event-meta? ke))
              (>= (char->integer ch) 32)
              (not (char=? ch #\rubout))))))

;; ============================================================
;; Tests
;; ============================================================

(module+ test
  (require rackunit)

  (test-case "key-event->key: self-insert"
    (check-equal? (key-event->key (key-event #\a #f #f #f #f)) #\a)
    (check-equal? (key-event->key (key-event #\space #f #f #f #f)) #\space))

  (test-case "key-event->key: C-char"
    (check-equal? (key-event->key (key-event #\a #t #f #f #f)) 'C-a)
    (check-equal? (key-event->key (key-event #\f #t #f #f #f)) 'C-f)
    (check-equal? (key-event->key (key-event #\space #t #f #f #f)) 'C-SPC))

  (test-case "key-event->key: M-char"
    (check-equal? (key-event->key (key-event #\x #f #t #f #f)) 'M-x))

  (test-case "key-event->key: named keys"
    (check-equal? (key-event->key (key-event #f #f #f #f 'up)) 'up)
    (check-equal? (key-event->key (key-event #f #f #f #f 'return)) 'return)
    (check-equal? (key-event->key (key-event #f #f #f #f 'tab)) 'tab)
    (check-equal? (key-event->key (key-event #f #f #f #f 'backspace)) 'backspace)
    (check-equal? (key-event->key (key-event #f #t #f #f 'up)) 'C-up))

  (test-case "key-event->key: null"
    (check-false (key-event->key (key-event #f #f #f #f #f))))

  ;; -- keymap ops --

  (test-case "keymap-set!/ref/remove!"
    (define km (make-keymap))
    (define cmd-a (command 'a (lambda (db evt) db)))
    (keymap-set! km 'C-a cmd-a)
    (check-eq? (keymap-ref km 'C-a) cmd-a)
    (check-false (keymap-ref km 'C-b))
    (keymap-remove! km 'C-a)
    (check-false (keymap-ref km 'C-a)))

  (test-case "keymap-empty? and keys"
    (define km (make-keymap))
    (check-true (keymap-empty? km))
    (keymap-set! km 'C-x (make-keymap))
    (check-false (keymap-empty? km))
    (check-true (pair? (memq 'C-x (keymap-keys km)))))

  ;; -- merge --

  (test-case "keymap-merge! overwrites"
    (define dst (make-keymap))
    (define src (make-keymap))
    (define cmd1 (command 'one (lambda (db evt) db)))
    (define cmd2 (command 'two (lambda (db evt) db)))
    (keymap-set! dst 'C-a cmd1)
    (keymap-set! dst 'C-b cmd1)
    (keymap-set! src 'C-b cmd2)
    (keymap-set! src 'C-c cmd2)
    (keymap-merge! dst src)
    (check-eq? (keymap-ref dst 'C-a) cmd1)
    (check-eq? (keymap-ref dst 'C-b) cmd2)
    (check-eq? (keymap-ref dst 'C-c) cmd2))

  (test-case "merge order: later wins"
    (define dst (make-keymap))
    (define a (make-keymap))
    (keymap-set! a 'C-f (command 'a-fn (lambda (db e) db)))
    (define b (make-keymap))
    (keymap-set! b 'C-f (command 'b-fn (lambda (db e) db)))
    (keymap-merge! dst a)
    (keymap-merge! dst b)
    (check-eq? (command-name (keymap-ref dst 'C-f)) 'b-fn))

  ;; -- sequence binding --

  (test-case "keymap-bind-key! single key"
    (define km (make-keymap))
    (define cmd (command 'find-file (lambda (db evt) db)))
    (keymap-bind-key! km '(C-f) cmd)
    (check-eq? (keymap-ref km 'C-f) cmd))

  (test-case "keymap-bind-key! two-key sequence"
    (define km (make-keymap))
    (define cmd (command 'find-file (lambda (db evt) db)))
    (keymap-bind-key! km '(C-x C-f) cmd)
    (define sub (keymap-ref km 'C-x))
    (check-true (keymap? sub))
    (check-eq? (keymap-ref sub 'C-f) cmd))

  (test-case "keymap-bind-key! three-key sequence"
    (define km (make-keymap))
    (define cmd (command 'deep (lambda (db evt) db)))
    (keymap-bind-key! km '(C-x C-c C-d) cmd)
    (define sub1 (keymap-ref km 'C-x))
    (define sub2 (keymap-ref sub1 'C-c))
    (check-true (keymap? sub2))
    (check-eq? (keymap-ref sub2 'C-d) cmd))

  (test-case "conflict: final key already a prefix"
    (define km (make-keymap))
    (define cmd (command 'c (lambda (db e) db)))
    ;; First make C-x a prefix
    (keymap-bind-key! km '(C-x C-f) cmd)
    ;; Now try to bind C-x directly as a command
    (check-exn exn:fail?
      (lambda () (keymap-bind-key! km '(C-x) cmd))))

  (test-case "conflict: intermediate key already a command"
    (define km (make-keymap))
    (define cmd (command 'c (lambda (db e) db)))
    ;; First bind C-x to a command
    (keymap-bind-key! km '(C-x) cmd)
    ;; Now try to make C-x a prefix
    (check-exn exn:fail?
      (lambda () (keymap-bind-key! km '(C-x C-f) cmd))))

  (test-case "keymap-bind-key! reuses existing prefix map"
    (define km (make-keymap))
    (define cmd-f  (command 'find-file  (lambda (db e) db)))
    (define cmd-b  (command 'buffer-list (lambda (db e) db)))
    (keymap-bind-key! km '(C-x C-f) cmd-f)
    (keymap-bind-key! km '(C-x C-b) cmd-b)
    (define sub (keymap-ref km 'C-x))
    (check-true (keymap? sub))
    (check-eq? (keymap-ref sub 'C-f) cmd-f)
    (check-eq? (keymap-ref sub 'C-b) cmd-b))

  ;; -- self-insert --

  (test-case "self-insert-key-event?"
    (check-true  (self-insert-key-event? (key-event #\a #f #f #f #f)))
    (check-false (self-insert-key-event? (key-event #\a #t #f #f #f)))
    (check-false (self-insert-key-event? (key-event #\x #f #t #f #f)))
    (check-false (self-insert-key-event? (key-event #\rubout #f #f #f #f)))
    (check-false (self-insert-key-event? (key-event #f #f #f #f 'up)))))
