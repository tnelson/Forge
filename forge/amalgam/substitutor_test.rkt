#lang forge/core
(require "substitutor.rkt")
(require "test_helpers.rkt")
(require "forge_ex.rkt")

(run udt
     #:preds [isUndirectedTree]
     #:scope [(Node 7)])

; Checking substitution in base cases
(printf "TEST 1 ~n~n")
(substitute-formula (substitute-formula (no (& edges iden)) '() iden univ) '() univ iden)

; Checking substitution in replacing relation names base case 
(printf "TEST 2 ~n~n")
(substitute-formula (= edges (~ edges)) '() edges Node)

; Checking substitution in union case 
(printf "TEST 4 ~n~n")
(substitute-expr (+ edges iden) '() edges univ)

; Checking substitution in set minus case
(printf "TEST 5 ~n~n")
(substitute-expr (- iden (+ edges iden)) '() 'iden univ)

; Checking substitution in intersection case
(printf "TEST 6 ~n~n")
(substitute-expr (& iden (- iden (+ edges iden))) '() 'iden Node)

; Checking substitution in product case
(printf "TEST 7 ~n~n")
(substitute-expr (-> edges (-> edges Node)) '() edges Node)

; Checking substitution in join case
(printf "TEST 8 ~n~n")
(substitute-expr (join edges (-> edges (+ edges iden))) '() edges univ)

; Checking substitution in transitive closure case
(printf "TEST 9 ~n~n")
(substitute-expr (^ edges) '() edges 'z)

; Checking substitution in reflexive-transitive closure case
(printf "TEST 10 ~n~n")
(substitute-expr (* edges) '() edges 'z)

; Checking substitution in transpose case
(printf "TEST 11~n~n")
(substitute-expr (~ edges) '() edges 'z)

; Checking substitution in join case (more complicated)
(printf "TEST 12 ~n~n")
(define f-some-reaches-all (some ([x Node]) (all ([y Node]) (in y (join x (^ edges))))))
(substitute-formula f-some-reaches-all '() edges Node)

; Checking substitution in quantifier-var case. Should throw variable shadowing error.
; How to utilize check-eq? for cases that throw back an error? check-eq is undefined 
(printf "TEST 12 ~n~n")
(substitute-formula f-some-reaches-all '() 'x 'z)

; Checking substitution in quantifier-var case. Should not throw variable shadowing error. 
(printf "TEST 13 ~n~n")
(define free-x-reaches-all (all ([y Node]) (in y (join x (^ edges))))) ; do substitution here with x since x is free

; Checking substitution in set comprehension case. Should throw variable shadowing error. 
;(printf "TEST 14 ~n~n")
;(substitute-formula  (([n Node]) (some (join n edges)))) '() 'n 'k)