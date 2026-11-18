#lang forge/core

; This is a "trivial" problem: the solver doesn't need to be called, because 
; translation produced true or false.  Still, we want symmetry breaking to 
; produce all the non-isomorphic instances of the problem.  
; (This is a regression test, although there is no issue number.)

(require (prefix-in ru: rackunit))

(set-option! 'verbose 0)

(sig A)
(sig B #:one)
(sig C #:one #:extends A)
(relation f (A B))
(relation g (B B B))

(run onechild #:scope ([A 2 2]))

(define (count-instances tree)
  (if (forge:Sat? (tree:get-value tree))
      (add1 (count-instances (tree:get-child tree 'next)))
      0))

(ru:check-equal? (count-instances (forge:Run-result onechild)) 8)
