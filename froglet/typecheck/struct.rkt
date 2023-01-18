#lang racket/base

(provide
  (struct-out type)
  (struct-out nametype)
  (struct-out consttype)
  (struct-out sigtype)
  (struct-out predtype)
  (struct-out fieldtype)
  (struct-out paramtype)

  (struct-out reltype))

(require
  syntax/parse/define
  (for-syntax racket/base racket/syntax))

;; ---

(define-simple-macro (struct/froglet id x ...)
  #:with make-id (format-id this-syntax "make-~a" #'id)
  (struct id x ... #:transparent #:extra-constructor-name make-id))

(struct/froglet type (name))
(struct/froglet nametype type ())
(struct/froglet consttype type ())
(struct/froglet sigtype type (mult extends field*))
(struct/froglet predtype type (param*))
(struct/froglet fieldtype type (mult sig))
(struct/froglet paramtype type (mult sig))

(struct/froglet reltype (col-type* singleton?))
;; reltype type (arity singleton?) ;; relation

