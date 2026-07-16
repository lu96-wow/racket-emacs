#lang racket

;; api/mode.rkt — Mode registry with pattern-matching keymap composition
;;
;; Modes match buffer filenames by pattern (e.g. ".rkt" matches "foo.rkt").
;; When a buffer gets a filename, update-buffer-keymap! composes
;; global + matching mode keymaps into the buffer's effective keymap.
;;
;; Composition: global → mode1 → mode2 → ...
;;   Later keymaps override earlier bindings for the same key.

(require "keymap.rkt"
         "command.rkt"
         "lang.rkt"
         "../kernel/buffer.rkt")

(provide
 ;; editor-mode type
 editor-mode? editor-mode
 editor-mode-name editor-mode-keymap editor-mode-pattern

 ;; registry
 register-mode!
 modes-for-buffer

 ;; buffer keymap
 update-buffer-keymap!
 init-buffer-with-filename!)

;; ============================================================
;; Mode struct
;; ============================================================

(struct editor-mode (name keymap pattern) #:transparent)
;; name    — symbol, e.g. 'racket
;; keymap  — hash of key → command
;; pattern — string, ".rkt" (matched as substring of filename)

;; ============================================================
;; Mode registry
;; ============================================================

(define mode-registry (box '()))

(define (register-mode! m)
  (set-box! mode-registry (cons m (unbox mode-registry))))

(define (modes-for-buffer buf)
  (define fname (buffer-filename buf))
  (define matches '())
  (for ([m (in-list (unbox mode-registry))])
    (define pat (editor-mode-pattern m))
    (when (and pat fname (string-contains? fname pat))
      (set! matches (cons m matches))))
  (reverse matches))

;; ============================================================
;; Keymap composition
;; ============================================================

(define (compose-keymaps kms)
  (define result (make-keymap))
  (for ([km (in-list kms)])
    (for ([(k v) (in-hash (keymap-hash km))])
      (keymap-set! result k v)))
  result)

;; ============================================================
;; update-buffer-keymap! — recompute effective keymap for buf
;; ============================================================

(define (update-buffer-keymap! buf)
  (define modes (modes-for-buffer buf))
  (define kms (cons global-keymap
                    (for/list ([m (in-list modes)])
                      (editor-mode-keymap m))))
  (set-buffer-keymap! buf (compose-keymaps kms)))

;; ============================================================
;; init-buffer-with-filename! — set filename + update keymap
;; ============================================================

(define (init-buffer-with-filename! buf fname)
  (set-buffer-filename! buf fname)
  (update-buffer-keymap! buf)
  (update-buffer-font-lock! buf))
