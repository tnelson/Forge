#lang typed/racket/base

; Notes for exporting on typed racket experiment
;   - no support for for/first, needed to rewrite with for/or
;   - the error messages are _terrible_. something about macros when i try to use for/first
;     and a _mismatched paren error_ i can't find a mismatched paren for. Magic Racket
;     highlights the wrong file. Lesson: needed to recompile via DrRacket (even racket filename)
;     wouldn't work.
;   - need to explicitly import super-structs; confusing runtime error about contract violation otherwise
;   - need to change from (provide (all-defined-out)) to something more explicit

(require racket/generator)
(require/typed forge/lang/ast 
  [relation-arity (-> Any Integer)]
  [#:struct node ([info : Any])]
  [#:struct (node/expr node) ([arity : Number])]
  [#:struct (node/expr/relation node/expr)
     ([name : String] 
      [typelist-thunk : Any] 
      [parent : Any] 
      [is-variable : Boolean])])
(require (only-in racket cartesian-product))

;; TODO: issue with doubly-required identifiers when transitioned to typed on this module
;; experienced problems even when trying to use except-out. Need to be very explicit.
;; (provide (all-defined-out))
(provide (struct-out bound) (struct-out bounds) 
           make-bound make-exact-bound exact-bound? make-upper-bound make-product-bound
           get-upper-bound bounds-variables bounds-union)

(define-type Tuple (Listof Any))

; A bound is a relation and two lists of tuples `lower` and `upper`.
(struct bound ([relation : node/expr/relation]
               [lower : (Listof Tuple)]
               [upper : (Listof Tuple)]) #:transparent)
; A bounds object is a universe and a collection of bound? instances.
(struct bounds ([universe : (Listof Any)]
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

(: make-product-bound (-> node/expr/relation (Listof Tuple) bound))
(define (make-product-bound relation col1 . columns)
  (make-bound relation '() (apply cartesian-product col1 columns)))

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
