#lang racket

;; api/lang.rkt — Language config composition
;;
;; Pure data: lang-config structs declare syntax-table + keywords + faces
;; matched by filename pattern.
;;
;; Composition mirrors api/mode.rkt:
;;   lang-config → register-lang! → langs-for-buffer → compose-lang-configs
;;   → update-buffer-font-lock!
;;
;; Each lang-config is data; composition is a pure function; application
;; is a single call.  No imperative setup-xxx-lang! functions.

(require "../kernel/syntax.rkt"
         "../kernel/buffer.rkt"
         "../base/font-lock.rkt"
         "../display/face.rkt")

(provide
 ;; lang-config — module-level data declaration
 lang-config? lang-config
 lang-config-name
 lang-config-patterns
 lang-config-syntax-table
 lang-config-keywords
 lang-config-faces

 ;; registry
 register-lang!
 langs-for-buffer

 ;; composition (pure)
 compose-lang-configs

 ;; apply (imperative, single entry point)
 update-buffer-font-lock!)

;; ============================================================
;; lang-config — pure data, no behaviour
;; ============================================================

(struct lang-config
  (name           ; symbol — 'racket, 'fundamental
   patterns        ; (listof string?) — filename patterns (".rkt" ".scrbl")
   syntax-table   ; syntax-table? | #f
   keywords        ; (listof (cons pregexp? symbol?))
   faces           ; (listof (list/c symbol? face-attrs?))
   case-fold?)     ; boolean?
  #:transparent)

;; ============================================================
;; Registry — like mode registry
;; ============================================================

(define lang-registry (box '()))

(define (register-lang! lc)
  (set-box! lang-registry (cons lc (unbox lang-registry))))

(define (langs-for-buffer buf)
  ;; Match buffer filename against pattern list.
  ;; Returns (listof lang-config?) in registration order.
  (define fname (buffer-filename buf))
  (define matches '())
  (for ([lang (in-list (unbox lang-registry))])
    (for ([pat (in-list (lang-config-patterns lang))])
      (when (and pat fname (string-contains? fname pat))
        (set! matches (cons lang matches)))))
  (reverse matches))

;; ============================================================
;; compose-lang-configs — pure: lang-config* → font-lock-config
;; ============================================================
;; Later configs in the list override earlier ones:
;;   syntax-table: last non-#f wins
;;   keywords:     later prepended (higher priority)
;;   case-fold?:   last one wins

(define (compose-lang-configs . langs)
  (define st #f)
  (define kws '())
  (define cf? #f)
  (define faces '())
  (for ([lang (in-list langs)])
    (when (lang-config-syntax-table lang)
      (set! st (lang-config-syntax-table lang)))
    (set! kws (append (lang-config-keywords lang) kws))
    (set! cf? (lang-config-case-fold? lang))
    (set! faces (append (lang-config-faces lang) faces)))
  ;; Register all collected faces
  (for ([f (in-list faces)])
    (match-define (list name attrs) f)
    (define-face! name attrs))
  (make-font-lock-config
   #:syntax-table st
   #:keywords kws
   #:case-fold? cf?))

;; ============================================================
;; update-buffer-font-lock! — compose + apply to buffer
;; ============================================================
;; Called after setting buffer filename (like update-buffer-keymap!).

(define (update-buffer-font-lock! buf)
  (define langs (langs-for-buffer buf))
  (unless (null? langs)
    (define config (apply compose-lang-configs langs))
    (set-buffer-font-lock-config! buf config)
    (fontify-buffer! buf)))
