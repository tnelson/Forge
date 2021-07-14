#lang racket

(require "ast.rkt")

(define (check-node-formula-constant formula-node)
  (void))

(define (check-node-formula-op formula-node)
  (void))

(define (check-node-formula-multiplicity formula-node)
  (void))

(define (check-node-formula-quantified formula-node)
  (void))

(define (check-node-formula-op-always formula-node)
  (void))

(define (check-node-formula-op-eventually formula-node)
  (void))

(define (check-node-formula-op-until formula-node)
  (void))

(define (check-node-formula-op-releases formula-node)
  (void))

(define (check-node-formula-op-after formula-node)
  (void))

(define (check-node-formula-op-historically formula-node)
  (void))

(define (check-node-formula-op-once formula-node)
  (void))

(define (check-node-formula-op-before formula-node)
  (void))

(define (check-node-formula-op-since formula-node)
  (void))

(define (check-node-formula-op-triggered formula-node)
  (void))

(define (check-node-formula-op-&& formula-node)
  (void))

(define (check-node-formula-op-|| formula-node)
  (void))

(define (check-node-formula-op-=> formula-node)
  (void))

(define (check-node-formula-op-in formula-node)
  (void))

(define (check-node-formula-op-= formula-node)
  (void))

(define (check-node-formula-op-! formula-node)
  (void))

(define (check-node-formula-op-int> formula-node)
  (void))

(define (check-node-formula-op-int< formula-node)
  (void))

(define (check-node-formula-op-int= formula-node)
  (void))

(define (check-node-expr-relation expr-node)
  (void))

(define (check-node-expr-atom expr-node)
  (void))

(define (check-node-expr-ite expr-node)
  (void))

(define (check-node-expr-constant expr-node)
  (void))

(define (check-node-expr-op expr-node)
  (void))

(define (check-node-expr-quantifier-var expr-node)
  (void))

(define (check-node-expr-comprehension expr-node)
  (void))

(define (check-node-expr-op-prime expr-node)
  (void))

(define (check-node-expr-op-+ expr-node)
  (void))

(define (check-node-expr-op-<: expr-node)
  (void))

(define (check-node-expr-op-- expr-node)
  (void))

(define (check-node-expr-op-& expr-node)
  (void))

(define (check-node-expr-op--> expr-node)
  (void))

(define (check-node-expr-op-join expr-node)
  (define left-hand-side (first (node/expr/op-children expr-node)))
  (unless (and (equal? 1 (node/expr-arity left-hand-side))
               (node/expr/relation? left-hand-side))
    (raise-user-error "Left hand side of join must be sig name")))

(define (check-node-expr-op-^ expr-node)
  (void))

(define (check-node-expr-op-* expr-node)
  (void))

(define (check-node-expr-op-~ expr-node)
  (void))

(define (check-node-expr-op-sing expr-node)
  (void))

(define checker-hash (make-hash))
(hash-set! checker-hash node/formula/constant check-node-formula-constant)
(hash-set! checker-hash node/formula/op check-node-formula-op)
(hash-set! checker-hash node/formula/multiplicity check-node-formula-multiplicity)
(hash-set! checker-hash node/formula/quantified check-node-formula-quantified)
(hash-set! checker-hash node/formula/op/always check-node-formula-op-always)
(hash-set! checker-hash node/formula/op/eventually check-node-formula-op-eventually)
(hash-set! checker-hash node/formula/op/until check-node-formula-op-until)
(hash-set! checker-hash node/formula/op/releases check-node-formula-op-releases)
(hash-set! checker-hash node/formula/op/after check-node-formula-op-after)
(hash-set! checker-hash node/formula/op/historically check-node-formula-op-historically)
(hash-set! checker-hash node/formula/op/once check-node-formula-op-once)
(hash-set! checker-hash node/formula/op/before check-node-formula-op-before)
(hash-set! checker-hash node/formula/op/since check-node-formula-op-since)
(hash-set! checker-hash node/formula/op/triggered check-node-formula-op-triggered)
(hash-set! checker-hash node/formula/op/&& check-node-formula-op-&&)
(hash-set! checker-hash node/formula/op/|| check-node-formula-op-||)
(hash-set! checker-hash node/formula/op/=> check-node-formula-op-=>)
(hash-set! checker-hash node/formula/op/in check-node-formula-op-in)
(hash-set! checker-hash node/formula/op/= check-node-formula-op-=)
(hash-set! checker-hash node/formula/op/! check-node-formula-op-!)
(hash-set! checker-hash node/formula/op/int> check-node-formula-op-int>)
(hash-set! checker-hash node/formula/op/int< check-node-formula-op-int<)
(hash-set! checker-hash node/formula/op/int= check-node-formula-op-int=)
(hash-set! checker-hash node/expr/relation check-node-expr-relation)
(hash-set! checker-hash node/expr/atom check-node-expr-atom)
(hash-set! checker-hash node/expr/ite check-node-expr-ite)
(hash-set! checker-hash node/expr/constant check-node-expr-constant)
(hash-set! checker-hash node/expr/op check-node-expr-op)
(hash-set! checker-hash node/expr/quantifier-var check-node-expr-quantifier-var)
(hash-set! checker-hash node/expr/comprehension check-node-expr-comprehension)
(hash-set! checker-hash node/expr/op/prime check-node-expr-op-prime)
(hash-set! checker-hash node/expr/op/+ check-node-expr-op-+)
(hash-set! checker-hash node/expr/op/<: check-node-expr-op-<:)
(hash-set! checker-hash node/expr/op/- check-node-expr-op--)
(hash-set! checker-hash node/expr/op/& check-node-expr-op-&)
(hash-set! checker-hash node/expr/op/-> check-node-expr-op-->)
(hash-set! checker-hash node/expr/op/join check-node-expr-op-join)
(hash-set! checker-hash node/expr/op/^ check-node-expr-op-^)
(hash-set! checker-hash node/expr/op/* check-node-expr-op-*)
(hash-set! checker-hash node/expr/op/~ check-node-expr-op-~)
(hash-set! checker-hash node/expr/op/sing check-node-expr-op-sing)

(provide checker-hash)
