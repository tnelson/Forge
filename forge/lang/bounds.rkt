#lang typed/racket/base

(require forge/types/ast-adapter)
(require forge/shared)

(provide (struct-out bound) (struct-out bounds) 
           make-bound make-exact-bound exact-bound? make-upper-bound 
           get-upper-bound bounds-variables bounds-union)

; A bound is a relation and two lists of tuples `lower` and `upper`.
(struct bound ([relation : node/expr/relation]
               [lower : (Listof Tuple)]
               [upper : (Listof Tuple)]) #:transparent)
; A bounds object is a universe and a collection of bound? instances.
(struct bounds ([universe : (Listof FAtom)]
                [entries : (Listof bound)]) #:transparent)

; Error-checking constructors for bounds
(: make-bound (-> node/expr/relation (Listof Tuple) (Listof Tuple) bound))
(define (make-bound relation lower upper)
  ;(printf "make-bound; upper was: ~a~n" upper)
  (for ([t (in-list lower)])
    (unless (and (list? t) (= (length t) (relation-arity relation)))
      (raise-arguments-error 'make-bound (format "lower bounds must contain tuples of arity ~a" (relation-arity relation)) "lower" t)))
  (for ([t (in-list upper)])
    (unless (and (list? t) (= (length t) (relation-arity relation)))
      (raise-arguments-error 'make-bound (format "upper bounds must contain tuples of arity ~a" (relation-arity relation)) "upper" t)))
  (bound relation lower upper))

(: make-exact-bound (-> node/expr/relation (Listof Tuple) bound))
(define (make-exact-bound relation contents)
  (make-bound relation contents contents))

(: exact-bound? (-> bound Boolean))
(define (exact-bound? b) (equal? (bound-lower b) (bound-upper b)))

(: make-upper-bound (-> node/expr/relation (Listof Tuple) bound))
(define (make-upper-bound relation contents)
  (make-bound relation '() contents))

; (: make-product-bound (-> node/expr/relation (Listof Tuple) (Listof Tuple) * bound))
; (define (make-product-bound relation col1 . columns)
;   (define ub (apply cartesian-product col1 columns))
;   (make-bound relation '() ub))

;; for/first is not supported by the typechecker??
;; but for/last is???

; Get the upper bound for a relation r in a bounds? object
(: get-upper-bound (-> bounds node/expr/relation (Option (Listof Tuple))))
(define (get-upper-bound bnds r)
  (for/or ([b (in-list (bounds-entries bnds))])
    (if (equal? (bound-relation b) r) 
        (bound-upper b)
        #f)))

; get a list of all relations bound by a bounds? object
(: bounds-variables (-> bounds (Listof Any)))
(define (bounds-variables bnds)
  (for/list : (Listof Any) ([b (in-list (bounds-entries bnds))]) (bound-relation b)))

; Combine several sets of bounds, which must be mutually disjoint and share the
; same universe
(: bounds-union (-> bounds bounds))
(define (bounds-union . lbnds)
  (define U (bounds-universe (car lbnds)))
  (bounds U (for*/list : (Listof bound) ([bnds (in-list lbnds)]
                        [bnd (in-list (bounds-entries bnds))]) bnd)))

