#lang forge/core
(require "substitutor.rkt")
(require "test_helpers.rkt")
(require "forge_ex.rkt")
(require (prefix-in @ rackunit))

(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)])

(define varz (node/expr/quantifier-var empty-nodeinfo 1 'z))
(define varx (node/expr/quantifier-var empty-nodeinfo 1 'x))
(define varz-arity2 (node/expr/quantifier-var empty-nodeinfo 2 'z))

; Checking substitution in base cases
(printf "TEST 1 ~n~n")
(@check-equal?
 (to-string (substitute-formula (substitute-formula (no (& edges iden)) '() iden univ) '() univ iden))
 (to-string (no (& edges iden))))

; Checking substitution in replacing relation names base case
(printf "TEST 2 ~n~n")
(@check-equal?
 (to-string (substitute-formula (= edges (~ edges)) '() edges varz-arity2))
 (to-string (= varz-arity2 (~ varz-arity2))))

; Checking substitution in union case
(printf "TEST 3 ~n~n")
(@check-equal?
 (to-string (substitute-expr (+ edges iden) '() edges iden))
 (to-string (+ iden iden)))

; Checking substitution in set minus case
(printf "TEST 4 ~n~n")
(@check-equal?
 (to-string (substitute-expr (- iden (+ edges iden)) '() edges iden))
 (to-string (- iden (+ iden iden))))

; Checking substitution in intersection case
(printf "TEST 5 ~n~n")
(@check-equal?
 (to-string (substitute-expr (& edges (- edges (+ edges edges))) '() edges iden))
 (to-string (& iden (- iden (+ iden iden)))))

; Checking substitution in product case
(printf "TEST 6 ~n~n")
(@check-equal?
 (to-string (substitute-expr (-> edges (-> edges Node)) '() Node univ))
 (to-string (-> edges (-> edges univ))))

; Checking substitution in join case
(printf "TEST 7 ~n~n")
(@check-equal?
 (to-string (substitute-expr (join edges iden) '() iden edges))
 (to-string (join edges edges)))

; Checking substitution in transitive closure case
(printf "TEST 8 ~n~n")
(@check-equal?
 (to-string (substitute-expr (^ edges) '() edges varz-arity2))
 (to-string (^ varz-arity2)))

; Checking substitution in reflexive-transitive closure case
(printf "TEST 9 ~n~n")
(@check-equal?
 (to-string (substitute-expr (* edges) '() edges varz-arity2))
 (to-string (* varz-arity2)))

; Checking substitution in transpose case
(printf "TEST 10 ~n~n")
(@check-equal?
 (to-string (substitute-expr (~ edges) '() edges varz-arity2))
 (to-string (~ varz-arity2)))

; Checking substitution in join case (more complicated)
(printf "TEST 11 ~n~n")
(define f-some-reaches-all (some ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))
(substitute-formula f-some-reaches-all '() edges Node)

(@check-equal?
 (to-string (substitute-formula (~ edges) '() edges varz-arity2))
 (to-string (~ varz-arity2)))

; Checking substitution in quantifier-var case. Should not throw variable shadowing error. 
(printf "TEST 12 ~n~n")
(define free-x-reaches-all (all ([y Node]) (in y (join varx (^ edges)))))
(substitute-formula free-x-reaches-all '() varx varz)

; Checking substitution in and case
(printf "TEST 13 ~n~n")
(define f-some-reaches-all-complicated (and
                                        (some ([x Node]) (all ([y Node]) (in y (join x (^ edges)))))
                                        (all ([y Node]) (in y (join varx (^ edges))))))
(substitute-formula f-some-reaches-all-complicated '() edges varz)

; Checking substitution in quantifier-var case. Should throw variable shadowing error.
; TODO: In order to throw an error in a test case, check-exn -- look at example
; need to wrap code in a lambda of no arguments. Rather than passing the expression that is your code,
; we give it a thunk (function of no arguments) to delay evaluation 
;(printf "TEST 14 ~n~n")
;(substitute-formula f-some-reaches-all '() varx varz) 

; Checking substitution in set comprehension case. Should throw variable shadowing error. 
;(printf "TEST 15 ~n~n")
;(substitute-formula (varx (some varx)) '() varx varz)