#lang forge/core
(require "substitutor.rkt")
(require "test_helpers.rkt")
(require "forge_ex.rkt")
(require (prefix-in @ rackunit))
(require debug/repl)
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
(@check-equal?
 (to-string (substitute-formula f-some-reaches-all '() edges varz-arity2))
 (to-string (some ([x Node]) (all ([y Node]) (in y (join x (^ varz-arity2)))))))

; Checking substitution in quantifier-var case. Should not throw variable shadowing error. 
(printf "TEST 12 ~n~n")
(define free-x-reaches-all (all ([y Node]) (in y (join varx (^ edges)))))
(@check-equal?
 (to-string (substitute-formula free-x-reaches-all '() varx varz))
 (to-string (all ([y Node]) (in y (join varz (^ edges))))))

; Checking substitution in and case
(printf "TEST 13 ~n~n")
(define f-some-reaches-all-complicated (and
                                        (some ([x Node]) (all ([y Node]) (in y (join x (^ edges)))))
                                        (all ([y Node]) (in y (join varx (^ edges))))))
(substitute-formula f-some-reaches-all-complicated '() edges varz)
(@check-equal?
 (to-string (substitute-formula f-some-reaches-all-complicated '() edges varz-arity2))
 (to-string (and
             (some ([x Node]) (all ([y Node]) (in y (join x (^ varz-arity2)))))
             (all ([y Node]) (in y (join varx (^ varz-arity2)))))))

; Checking substitution in quantifier-var case. Should throw variable shadowing error.
; TODO: In order to throw an error in a test case, check-exn -- look at example
; need to wrap code in a lambda of no arguments. Rather than passing the expression that is your code,
; we give it a thunk (function of no arguments) to delay evaluation 
(printf "TEST 14 ~n~n")
(@check-exn
 exn:fail?
 (lambda () 
   (substitute-formula f-some-reaches-all '() varx varz)))

; Checking substitution in set comprehension case. Should throw variable shadowing error. 
;(printf "TEST 15 ~n~n")
;(substitute-formula (varx (some varx)) '() varx varz)

; formula constant
(define var-const-x (node/formula/constant empty-nodeinfo Int))
(define var-const-y (node/formula/constant empty-nodeinfo Int))
(printf "TEST 15 ~n~n")
(@check-equal?
 (to-string (substitute-formula var-const-x '() var-const-x var-const-y))
 (to-string var-const-y))
             
(printf "TEST 16 ~n~n")
(@check-equal?
 (to-string (substitute-formula var-const-x '() var-const-x var-const-x))
 (to-string var-const-x))

; Checking substitution in OR case
(printf "TEST 17 ~n~n")
(define f-some-reaches-all-complicated-or (or
                                           (some ([x Node]) (all ([y Node]) (in y (join x (^ edges)))))
                                           (all ([y Node]) (in y (join varx (^ edges))))))

(@check-equal?
 (to-string (substitute-formula f-some-reaches-all-complicated-or '() edges varz-arity2))
 (to-string (or
             (some ([x Node]) (all ([y Node]) (in y (join x (^ varz-arity2)))))
             (all ([y Node]) (in y (join varx (^ varz-arity2)))))))


; formula implies
(printf "TEST 18 ~n~n")
(define f-some-reaches-all-complicated-implies (implies
                                                (some ([x Node]) (all ([y Node]) (in y (join x (^ edges)))))
                                                (all ([y Node]) (in y (join varx (^ edges))))))
(@check-equal?
 (to-string (substitute-formula f-some-reaches-all-complicated-implies '() edges varz-arity2))
 (to-string (implies
             (some ([x Node]) (all ([y Node]) (in y (join x (^ varz-arity2)))))
             (all ([y Node]) (in y (join varx (^ varz-arity2)))))))


; formula in
(define f-some-reaches-all-simple-in (some ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))
(define f-some-reaches-all-complex-in (and
                                     (or (some ([x Node]) (all ([y Node]) (in y (join x (^ edges)))))
                                         (no ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))
                                     (all ([x Node]) (all ([y Node]) (in y (join x (^ edges)))))))

(printf "TEST 19 ~n~n")
(@check-equal?
 (to-string (substitute-formula f-some-reaches-all-simple-in '() edges varz-arity2))
 (to-string (some ([x Node]) (all ([y Node]) (in y (join x (^ varz-arity2)))))))

(printf "TEST 20 ~n~n")
(@check-equal?
 (to-string (substitute-formula f-some-reaches-all-complex-in '() edges varz-arity2))
 (to-string (and
             (or (some ([x Node]) (all ([y Node]) (in y (join x (^ varz-arity2)))))
                 (no ([x Node]) (all ([y Node]) (in y (join x (^ varz-arity2))))))
             (all ([x Node]) (all ([y Node]) (in y (join x (^ varz-arity2))))))))

; formula negation
(printf "TEST 21 ~n~n")
(define f-some-reaches-all-simple-negation (not (some ([x Node]) (all ([y Node]) (in y (join x (^ edges)))))))
(@check-equal?
 (to-string (substitute-formula f-some-reaches-all-simple-negation '() edges varz-arity2))
 (to-string (not (some ([x Node]) (all ([y Node]) (in y (join x (^ varz-arity2))))))))

; formula integer >
(printf "TEST 22 ~n~n")
(define var-int-const-x (node/int/constant empty-nodeinfo 1))
(define var-int-const-y (node/int/constant empty-nodeinfo 2))
(define f-int-greater (> var-int-const-x var-int-const-y))
(@check-exn
 exn:fail?
 (lambda () 
   (substitute-int f-int-greater '() var-int-const-x var-int-const-y)))

; formula integer <
(printf "TEST 23 ~n~n")
(define f-int-less (< var-int-const-x var-int-const-y))
(@check-exn
 exn:fail?
 (lambda () 
   (substitute-int f-int-less '() var-int-const-x var-int-const-y)))

; formula integer =
(printf "TEST 23 ~n~n")
(define f-int-equal (= var-int-const-x var-int-const-y))
(@check-exn
 exn:fail?
 (lambda () 
   (substitute-int f-int-equal '() var-int-const-x var-int-const-y)))

; operator
(printf "TEST 24 ~n~n")
(define f-int-plus (node/int/op/add empty-nodeinfo (list var-int-const-x var-int-const-y)))
(@check-exn
 exn:fail?
 (lambda () 
   (substitute-int f-int-plus '() var-int-const-x var-int-const-y)))

; cardinality
(printf "TEST 25 ~n~n")
(define f-cardinality (node/int/op/card empty-nodeinfo (list Node)))
(@check-equal?
 (to-string (substitute-int f-cardinality '() Node edges))
 (to-string (node/int/op/card empty-nodeinfo (list edges))))

; sum
; TODO: Finish this case. What is int-expr?
; e.g. sum p : Person | p.age
(printf "TEST 26 ~n~n")
#|(define x-node Node)
(define f-sum (node/int/sum-quant empty-nodeinfo (list (cons x 'Node)) (node/int/op/card empty-nodeinfo (list (join edges x)))))  
(@check-equal?
 (to-string (substitute-formula f-sum '() edges varz-arity2))
 (to-string  (node/int/sum-quant empty-nodeinfo (list (cons x 'Node)) (node/int/op/card empty-nodeinfo (list (join varz-arity2 x))))))|#
