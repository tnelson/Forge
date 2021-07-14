#lang racket

(require "ast.rkt")
(provide checker-hash)

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
  (void))

(define (check-node-expr-op-^ expr-node)
  (void))

(define (check-node-expr-op-* expr-node)
  (void))

(define (check-node-expr-op-~ expr-node)
  (void))

(define (check-node-expr-op-sing expr-node)
  (void))

(define checker-hash
  (make-hash (list (list node/formula/constant check-node-formula-constant)
                   (list node/formula/op check-node-formula-op)
                   (list node/formula/multiplicity check-node-formula-multiplicity)
                   (list node/formula/quantified check-node-formula-quantified)
                   (list node/formula/op/always check-node-formula-op-always)
                   (list node/formula/op/eventually check-node-formula-op-eventually)
                   (list node/formula/op/until check-node-formula-op-until)
                   (list node/formula/op/releases check-node-formula-op-releases)
                   (list node/formula/op/after check-node-formula-op-after)
                   (list node/formula/op/historically check-node-formula-op-historically)
                   (list node/formula/op/once check-node-formula-op-once)
                   (list node/formula/op/before check-node-formula-op-before)
                   (list node/formula/op/since check-node-formula-op-since)
                   (list node/formula/op/triggered check-node-formula-op-triggered)
                   (list node/formula/op/&& check-node-formula-op-&&)
                   (list node/formula/op/|| check-node-formula-op-||)
                   (list node/formula/op/=> check-node-formula-op-=>)
                   (list node/formula/op/in check-node-formula-op-in)
                   (list node/formula/op/= check-node-formula-op-=)
                   (list node/formula/op/! check-node-formula-op-!)
                   (list node/formula/op/int> check-node-formula-op-int>)
                   (list node/formula/op/int< check-node-formula-op-int<)
                   (list node/formula/op/int= check-node-formula-op-int=)
                   (list node/expr/relation check-node-expr-relation)
                   (list node/expr/atom check-node-expr-atom)
                   (list node/expr/ite check-node-expr-ite)
                   (list node/expr/constant check-node-expr-constant)
                   (list node/expr/op check-node-expr-op)
                   (list node/expr/quantifier-var check-node-expr-quantifier-var)
                   (list node/expr/comprehension check-node-expr-comprehension)
                   (list node/expr/op/prime check-node-expr-op-prime)
                   (list node/expr/op/+ check-node-expr-op-+)
                   (list node/expr/op/<: check-node-expr-op-<:)
                   (list node/expr/op/- check-node-expr-op--)
                   (list node/expr/op/& check-node-expr-op-&)
                   (list node/expr/op/-> check-node-expr-op-->)
                   (list node/expr/op/join check-node-expr-op-join)
                   (list node/expr/op/^ check-node-expr-op-^)
                   (list node/expr/op/* check-node-expr-op-~)
                   (list node/expr/op/sing check-node-expr-op-sing))))
