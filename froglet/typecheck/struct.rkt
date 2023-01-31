#lang racket/base

(provide
  (struct-out type)
  (struct-out nametype)
  (struct-out consttype)
  (struct-out sigtype)
  (struct-out predtype)
  (struct-out fieldtype)
  (struct-out paramtype)

  (struct-out reltype)
  the-int-type
  the-bool-type
  the-unknown-type
  unknown-type?
  type-kind)

(require
  syntax/parse/define
  (for-syntax racket/base racket/syntax))

;; ---

(define-simple-macro (struct/froglet id x ...)
  #:with make-id (format-id this-syntax "make-~a" #'id)
  (struct id x ... #:prefab #:extra-constructor-name make-id))

(struct/froglet type (name))
(struct/froglet nametype type ())
(struct/froglet consttype type ())
(struct/froglet sigtype type (mult extends field*))
(struct/froglet predtype type (param*))
(struct/froglet fieldtype type (mult sig))
(struct/froglet paramtype type (mult sig))

(struct/froglet reltype (col-type* singleton?))
;; reltype type (arity singleton?) ;; relation

(define (symbol->type sym)
  (sigtype (datum->syntax #f sym) #f #f '()))

(define the-int-type (symbol->type 'Int))
(define the-bool-type (symbol->type 'Bool))
(define the-unknown-type (type (datum->syntax #f 'Unknown)))

(define (unknown-type? t)
  (eq? t the-unknown-type))

(define (type-kind tt)
  (cond
    [(nametype? tt) "name"]
    [(consttype? tt) "constant"]
    [(sigtype? tt) "sig"]
    [(predtype? tt) "pred"]
    [(fieldtype? tt) "field"]
    [(paramtype? tt) "parameter"]
    [(reltype? tt) "relation"]
    [(unknown-type? tt) "unknown-type"]
    [else "type"]))

