#lang racket/base

(require "ast.rkt")
(require forge/sigs-structs)
(require (for-syntax racket/syntax syntax/srcloc)
         syntax/srcloc)

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

(define (check-node-formula-op-next_state formula-node)
  (void))

(define (check-node-formula-op-historically formula-node)
  (void))

(define (check-node-formula-op-once formula-node)
  (void))

(define (check-node-formula-op-prev_state formula-node)
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

(define (check-node-expr-fun-spacer expr-node)
  (void))
(define (check-node-fmla-pred-spacer expr-node)
  (void))

(define forge-checker-hash (make-hash))
(hash-set! forge-checker-hash node/fmla/pred-spacer check-node-fmla-pred-spacer)
(hash-set! forge-checker-hash node/expr/fun-spacer check-node-expr-fun-spacer)
(hash-set! forge-checker-hash node/formula/constant check-node-formula-constant)
(hash-set! forge-checker-hash node/formula/op check-node-formula-op)
(hash-set! forge-checker-hash node/formula/multiplicity check-node-formula-multiplicity)
(hash-set! forge-checker-hash node/formula/quantified check-node-formula-quantified)
(hash-set! forge-checker-hash node/formula/op/always check-node-formula-op-always)
(hash-set! forge-checker-hash node/formula/op/eventually check-node-formula-op-eventually)
(hash-set! forge-checker-hash node/formula/op/until check-node-formula-op-until)
(hash-set! forge-checker-hash node/formula/op/releases check-node-formula-op-releases)
(hash-set! forge-checker-hash node/formula/op/next_state check-node-formula-op-next_state)
(hash-set! forge-checker-hash node/formula/op/historically check-node-formula-op-historically)
(hash-set! forge-checker-hash node/formula/op/once check-node-formula-op-once)
(hash-set! forge-checker-hash node/formula/op/prev_state check-node-formula-op-prev_state)
(hash-set! forge-checker-hash node/formula/op/since check-node-formula-op-since)
(hash-set! forge-checker-hash node/formula/op/triggered check-node-formula-op-triggered)
(hash-set! forge-checker-hash node/formula/op/&& check-node-formula-op-&&)
(hash-set! forge-checker-hash node/formula/op/|| check-node-formula-op-||)
(hash-set! forge-checker-hash node/formula/op/=> check-node-formula-op-=>)
(hash-set! forge-checker-hash node/formula/op/in check-node-formula-op-in)
(hash-set! forge-checker-hash node/formula/op/= check-node-formula-op-=)
(hash-set! forge-checker-hash node/formula/op/! check-node-formula-op-!)
(hash-set! forge-checker-hash node/formula/op/int> check-node-formula-op-int>)
(hash-set! forge-checker-hash node/formula/op/int< check-node-formula-op-int<)
(hash-set! forge-checker-hash node/formula/op/int= check-node-formula-op-int=)
(hash-set! forge-checker-hash node/expr/relation check-node-expr-relation)
(hash-set! forge-checker-hash node/expr/atom check-node-expr-atom)
(hash-set! forge-checker-hash node/expr/ite check-node-expr-ite)
(hash-set! forge-checker-hash node/expr/constant check-node-expr-constant)
(hash-set! forge-checker-hash node/expr/op check-node-expr-op)
(hash-set! forge-checker-hash node/expr/quantifier-var check-node-expr-quantifier-var)
(hash-set! forge-checker-hash node/expr/comprehension check-node-expr-comprehension)
(hash-set! forge-checker-hash node/expr/op/prime check-node-expr-op-prime)
(hash-set! forge-checker-hash node/expr/op/+ check-node-expr-op-+)
(hash-set! forge-checker-hash node/expr/op/- check-node-expr-op--)
(hash-set! forge-checker-hash node/expr/op/& check-node-expr-op-&)
(hash-set! forge-checker-hash node/expr/op/-> check-node-expr-op-->)
(hash-set! forge-checker-hash node/expr/op/join check-node-expr-op-join)
(hash-set! forge-checker-hash node/expr/op/^ check-node-expr-op-^)
(hash-set! forge-checker-hash node/expr/op/* check-node-expr-op-*)
(hash-set! forge-checker-hash node/expr/op/~ check-node-expr-op-~)
(hash-set! forge-checker-hash node/expr/op/sing check-node-expr-op-sing)

(provide forge-checker-hash)

(define (forge-ast-arg-checks args)
  (void))

(define forge-ast-checker-hash (make-hash))
(hash-set! forge-ast-checker-hash "check-args" forge-ast-arg-checks)

(provide forge-ast-checker-hash)

(define forge-inst-checker-hash (make-hash))
(provide forge-inst-checker-hash)