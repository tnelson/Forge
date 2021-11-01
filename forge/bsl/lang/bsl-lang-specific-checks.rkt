#lang racket

(require forge/lang/deparser)
(require forge/lang/ast)
(require forge/sigs-structs)
(require (for-syntax racket/syntax syntax/srcloc)
         syntax/srcloc (prefix-in @ racket) (prefix-in $ racket))

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
    ;((raise-user-error (format "Comprehension at ~a  " (deparse expr-node)))))

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
  ; ;(printf "checking join: ~a~n" expr-node)
  ; (define left-hand-side (first (node/expr/op-children expr-node)))
  ; (define loc (nodeinfo-loc (node-info left-hand-side)))
  ; (define locstr (format "line ~a, col ~a, span: ~a" (source-location-line loc) (source-location-column loc) (source-location-span loc)))
  ; (unless (or (node/expr/quantifier-var? left-hand-side)
  ;         (and (node/expr/relation? left-hand-side) (equal? 1 (node/expr-arity left-hand-side)) (Sig-one left-hand-side)))
  ;   (raise-user-error (format "Left hand side to field access at ~a must be a sig at loc: ~a" (deparse expr-node) locstr))))

(define (check-node-expr-op-^ expr-node)
  (void))

(define (check-node-expr-op-* expr-node)
  (void))

(define (check-node-expr-op-~ expr-node)
  (void))

(define (check-node-expr-op-sing expr-node)
  (void))

(define bsl-checker-hash (make-hash))
(hash-set! bsl-checker-hash node/formula/constant check-node-formula-constant)
(hash-set! bsl-checker-hash node/formula/op check-node-formula-op)
(hash-set! bsl-checker-hash node/formula/multiplicity check-node-formula-multiplicity)
(hash-set! bsl-checker-hash node/formula/quantified check-node-formula-quantified)
(hash-set! bsl-checker-hash node/formula/op/always check-node-formula-op-always)
(hash-set! bsl-checker-hash node/formula/op/eventually check-node-formula-op-eventually)
(hash-set! bsl-checker-hash node/formula/op/until check-node-formula-op-until)
(hash-set! bsl-checker-hash node/formula/op/releases check-node-formula-op-releases)
(hash-set! bsl-checker-hash node/formula/op/after check-node-formula-op-after)
(hash-set! bsl-checker-hash node/formula/op/historically check-node-formula-op-historically)
(hash-set! bsl-checker-hash node/formula/op/once check-node-formula-op-once)
(hash-set! bsl-checker-hash node/formula/op/before check-node-formula-op-before)
(hash-set! bsl-checker-hash node/formula/op/since check-node-formula-op-since)
(hash-set! bsl-checker-hash node/formula/op/triggered check-node-formula-op-triggered)
(hash-set! bsl-checker-hash node/formula/op/&& check-node-formula-op-&&)
(hash-set! bsl-checker-hash node/formula/op/|| check-node-formula-op-||)
(hash-set! bsl-checker-hash node/formula/op/=> check-node-formula-op-=>)
(hash-set! bsl-checker-hash node/formula/op/in check-node-formula-op-in)
(hash-set! bsl-checker-hash node/formula/op/= check-node-formula-op-=)
(hash-set! bsl-checker-hash node/formula/op/! check-node-formula-op-!)
(hash-set! bsl-checker-hash node/formula/op/int> check-node-formula-op-int>)
(hash-set! bsl-checker-hash node/formula/op/int< check-node-formula-op-int<)
(hash-set! bsl-checker-hash node/formula/op/int= check-node-formula-op-int=)
(hash-set! bsl-checker-hash node/expr/relation check-node-expr-relation)
(hash-set! bsl-checker-hash node/expr/atom check-node-expr-atom)
(hash-set! bsl-checker-hash node/expr/ite check-node-expr-ite)
(hash-set! bsl-checker-hash node/expr/constant check-node-expr-constant)
(hash-set! bsl-checker-hash node/expr/op check-node-expr-op)
(hash-set! bsl-checker-hash node/expr/quantifier-var check-node-expr-quantifier-var)
(hash-set! bsl-checker-hash node/expr/comprehension check-node-expr-comprehension)
(hash-set! bsl-checker-hash node/expr/op/prime check-node-expr-op-prime)
(hash-set! bsl-checker-hash node/expr/op/+ check-node-expr-op-+)
(hash-set! bsl-checker-hash node/expr/op/- check-node-expr-op--)
(hash-set! bsl-checker-hash node/expr/op/& check-node-expr-op-&)
(hash-set! bsl-checker-hash node/expr/op/-> check-node-expr-op-->)
(hash-set! bsl-checker-hash node/expr/op/join check-node-expr-op-join)
(hash-set! bsl-checker-hash node/expr/op/^ check-node-expr-op-^)
(hash-set! bsl-checker-hash node/expr/op/* check-node-expr-op-*)
(hash-set! bsl-checker-hash node/expr/op/~ check-node-expr-op-~)
(hash-set! bsl-checker-hash node/expr/op/sing check-node-expr-op-sing)

(provide bsl-checker-hash)


(define (bsl-ast-arg-checks args)
  (void))

; motivation: if student use join on relations and get arity error, that could be confusing
#;(define (check-node-expr-op-join-args expr-args)
  ;(printf "checking join: ~a~n" expr-args)
  (define left-hand-side (first expr-args))
  (define loc (nodeinfo-loc (node-info left-hand-side)))
  (define locstr (format "line ~a, col ~a, span: ~a" (source-location-line loc) (source-location-column loc) (source-location-span loc)))
  (unless (or (node/expr/quantifier-var? left-hand-side)
          (and (node/expr/relation? left-hand-side) (equal? 1 (node/expr-arity left-hand-side)) (Sig-one left-hand-side)))
    (raise-user-error (format "Left hand side to field access at ~a must be a sig at loc: ~a" expr-args locstr))))

  ;NOTE: make better error message 

(define (bsl-field-decl-func true-breaker)
  (unless (or (equal? 'func (node/breaking/break-break true-breaker)) (equal? 'pfunc (node/breaking/break-break true-breaker))) 
  (raise-user-error (format "Field declaration is not a function"))))



(define bsl-ast-checker-hash (make-hash))
(hash-set! bsl-ast-checker-hash "check-args" bsl-ast-arg-checks)
(hash-set! bsl-ast-checker-hash 'field-decl bsl-field-decl-func)
;(hash-set! bsl-ast-checker-hash node/expr/op/join check-node-expr-op-join-args)


(provide bsl-ast-checker-hash)