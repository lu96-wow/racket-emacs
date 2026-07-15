#lang racket

;; kernel/category.rkt — Semantic categories for text annotation
;;
;; Protocol between kernel analysis (font-lock, syntax) and display
;; rendering (face).  Kernel writes category symbols as text properties;
;; display maps them to visual attributes independently.
;;
;; Zero display dependency.  Zero platform dependency.

(provide
 ;; semantic categories
 category-string category-comment
 category-keyword category-builtin
 category-constant category-function-name
 category-type category-variable-name

 ;; paren-depth faces (rainbow brackets, 12 depth levels)
 paren-depth-face-1 paren-depth-face-2 paren-depth-face-3
 paren-depth-face-4 paren-depth-face-5 paren-depth-face-6
 paren-depth-face-7 paren-depth-face-8 paren-depth-face-9
 paren-depth-face-10 paren-depth-face-11 paren-depth-face-12
 paren-depth-faces)

;; ============================================================
;; Semantic categories
;; ============================================================
;; Each symbol names a semantic role for a span of text.
;; The display layer decides how to render each category.

(define category-string           'string)
(define category-comment          'comment)
(define category-keyword          'keyword)
(define category-builtin          'builtin)
(define category-constant         'constant)
(define category-function-name    'function-name)
(define category-type             'type)
(define category-variable-name    'variable-name)

;; ============================================================
;; Paren-depth faces — rainbow bracket protocol
;; ============================================================
;; Each level names a distinct bracket-nesting depth so the
;; renderer can tint bracket characters by depth.

(define paren-depth-face-1  'paren-depth-1)
(define paren-depth-face-2  'paren-depth-2)
(define paren-depth-face-3  'paren-depth-3)
(define paren-depth-face-4  'paren-depth-4)
(define paren-depth-face-5  'paren-depth-5)
(define paren-depth-face-6  'paren-depth-6)
(define paren-depth-face-7  'paren-depth-7)
(define paren-depth-face-8  'paren-depth-8)
(define paren-depth-face-9  'paren-depth-9)
(define paren-depth-face-10 'paren-depth-10)
(define paren-depth-face-11 'paren-depth-11)
(define paren-depth-face-12 'paren-depth-12)

(define paren-depth-faces
  (vector paren-depth-face-1
          paren-depth-face-2
          paren-depth-face-3
          paren-depth-face-4
          paren-depth-face-5
          paren-depth-face-6
          paren-depth-face-7
          paren-depth-face-8
          paren-depth-face-9
          paren-depth-face-10
          paren-depth-face-11
          paren-depth-face-12))
