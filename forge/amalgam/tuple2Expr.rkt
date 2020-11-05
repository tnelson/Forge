#lang racket

#|
Function tup2Expr that has the purpose of translating a given tuple (i.e. (Node0 Node1) to an expression (i.e. Node0->Node1)
|#

(require racket/base)
(require racket/list)

(define (tup2Expr tuple relation)
  (let ([tupRelationList '()])
   (map
    (lambda (tupElem) 
          (cons  
           (filter-map 
            (lambda (relEntry) (equal? (string tupElem) (forge:relation-name (forge:bound-relation relEntry))))
            relation)
           tupleRelationList))
    tuple))
  (node/expr/op/-> (length tupRelationList) tupRelationList))


(define rel '((relation 1 "-8" (-8) univ)
  (relation 1 "-7" (-7) univ)
  (relation 1 "-6" (-6) univ)
  (relation 1 "-5" (-5) univ)
  (relation 1 "-4" (-4) univ)
  (relation 1 "-3" (-3) univ)
  (relation 1 "-2" (-2) univ)
  (relation 1 "-1" (-1) univ)
  (relation 1 "0" (0) univ)
  (relation 1 "1" (1) univ)
  (relation 1 "2" (2) univ)
  (relation 1 "3" (3) univ)
  (relation 1 "4" (4) univ)
  (relation 1 "5" (5) univ)
  (relation 1 "6" (6) univ)
  (relation 1 "7" (7) univ)
  (relation 1 "Node0" (Node0) univ)
  (relation 1 "Node1" (Node1) univ)
  (relation 1 "Node2" (Node2) univ)
  (relation 1 "Node3" (Node3) univ)))

(define tup '(Node0 Node1))

(tup2Expr tup rel)


