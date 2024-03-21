#lang racket/base

(provide
  (struct-out type)
  (struct-out nametype)
  (struct-out consttype)
  (struct-out sigtype)
  (struct-out predtype)
  (except-out (struct-out funtype) make-funtype)
  (rename-out [-funtype make-funtype])
  (struct-out fieldtype)
  (struct-out paramtype)

  (struct-out reltype)
  the-int-type
  the-bool-type
  the-unknown-type
  unknown-type?
  type-kind
  sigtype-update-mult
  type-serialize
  type-equal?

  funtype-set-return!
  funtype->return)

(require
  syntax/parse/define
  (only-in froglet/util log-froglet-info log-froglet-warning)
  (for-syntax racket/base racket/syntax))

;; ---

;; TODO make updaters for all fields?

(define-simple-macro (struct/froglet id x ...)
  #:with make-id (format-id this-syntax "make-~a" #'id)
  (struct id x ... #:prefab #:extra-constructor-name make-id))

(struct/froglet type (name))
(struct/froglet nametype type ())
(struct/froglet consttype type (val))
(struct/froglet sigtype type (mult extends field*))
(struct/froglet predtype type (param*))
(struct/froglet funtype type (param* return))
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

(define (sigtype-update-mult st m)
  (sigtype
    (type-name st)
    m
    (sigtype-extends st)
    (sigtype-field* st)))

(define (type-serialize ty)
  (cond
    [(funtype? ty)
     (funtype-serialize ty)]
    [else
     ty]))

(define (funtype-serialize ty)
  (define rval (funtype-return ty))
  (funtype
    (type-name ty)
    (funtype-param* ty)
    (if (box? rval) (unbox rval) rval)))

(define (-funtype name param* [return #f])
  (funtype name param* (or return (box #f))))

(define (funtype-set-return! ft rt)
  (define rval (funtype-return ft))
  (if (and (box? rval) (not (unbox rval)))
    (set-box! rval rt)
    (log-froglet-warning "funtype-set-return!: no-op, ~a~n curr-ty: ~a~n new-ty: ~a"
                         (if (box? rval)
                           "return type already initialized"
                           "return type is immutable")
                         (if (box? rval) (unbox rval) rval)
                         rt)))

(define (funtype->return ft)
  (define rval (funtype-return ft))
  (if (box? rval)
    (or (unbox rval) the-unknown-type)
    rval))

(define (type-equal? t0 t1)
  (or (eq? t0 t1)
      (cond
        [(and (sigtype? t0) (sigtype? t1))
         (and
           (equal? (type-name t0) (type-name t1))
           (equal? (sigtype-extends t0) (sigtype-extends t1))
           (equal? (sigtype-field* t0) (sigtype-field* t1)))]
        [else
          #f])))

