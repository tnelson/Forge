#lang racket

(require rackunit)
(require "../server/eval-model.rkt")

(define (relation-eq? things relation)
  (equal? (list->set things) (list->set relation)))

(define-simple-check (check-eval-to? i b e r)
  (cond [(boolean? r) (equal? r (eval-unknown e b i))]
        [else (relation-eq? r (eval-unknown e b i))]))

; Must be an immutable hash, or eval-model will throw a contract violation when substituting.
(define binding1 (make-immutable-hash '(
                              (Node . ((Node0)
                                       (Node1)
                                       (Node2)))
                              (edges . ((Node0 Node1)
                                        (Node1 Node2)))
                              )))

(printf "-----------------------------~nRunning test-evaluator.rkt~n-----------------------------~n")

; Expected name-binding-expression-result tuples
(check-eval-to? 4 binding1 'edges               '((Node1 Node2) (Node0 Node1)))
(check-eval-to? 4 binding1 'Node                '((Node1) (Node2) (Node0)))
(check-eval-to? 4 binding1 '(join edges edges)  '((Node0 Node2)))
(check-eval-to? 4 binding1 '(join edges (join edges edges))  '())
(check-eval-to? 4 binding1 '(^ edges)           '((Node0 Node1) (Node1 Node2) (Node0 Node2)))
(check-eval-to? 4 binding1 '(* edges)           '((Node0 Node1) (Node1 Node2) (Node0 Node2) (Node0 Node0) (Node1 Node1) (Node2 Node2)))
(check-eval-to? 4 binding1 '(~ edges)           '((Node2 Node1) (Node1 Node0)))
(check-eval-to? 4 binding1 '(+ edges (~ edges)) '((Node1 Node2) (Node0 Node1) (Node2 Node1) (Node1 Node0)))
(check-eval-to? 4 binding1 '(- edges (~ edges)) '((Node1 Node2) (Node0 Node1)))
(check-eval-to? 4 binding1 'univ                '((Node0) (Node1) (Node2)))
(check-eval-to? 4 binding1 'iden                '((Node0 Node0) (Node1 Node1) (Node2 Node2)))
(check-eval-to? 4 binding1 'none                '())            

(check-eval-to? 4 binding1 '(= edges edges)     #t)
(check-eval-to? 4 binding1 '(= edges (~ edges)) #f)
(check-eval-to? 4 binding1 '(some edges)        #t)
(check-eval-to? 4 binding1 '(no edges)          #f)
(check-eval-to? 4 binding1 '(lone edges)        #f)
(check-eval-to? 4 binding1 '(one edges)         #f)
(check-eval-to? 4 binding1 '(one (join edges edges))     #t)
(check-eval-to? 4 binding1 '(lone (join edges edges))    #t)
(check-eval-to? 4 binding1 '(no (join none edges))       #t)
(check-eval-to? 4 binding1 '(some x Node (some (join x edges))) #t)
(check-eval-to? 4 binding1 '(all x Node (some (join x edges))) #f)

(check-eval-to? 4 binding1 7   '((7)))
(check-eval-to? 4 binding1 8   '((-8)))
(check-eval-to? 4 binding1 -9  '((7)))
(check-eval-to? 5 binding1 7   '((7)))
(check-eval-to? 5 binding1 8   '((8)))
(check-eval-to? 5 binding1 -9  '((-9)))
(check-eval-to? 5 binding1 15   '((15)))
(check-eval-to? 5 binding1 16   '((-16)))
(check-eval-to? 5 binding1 -17  '((15)))

(check-eval-to? 5 binding1 '(plus 4 4) '((8)))
(check-eval-to? 5 binding1 '(minus 4 4) '((0)))
(check-eval-to? 5 binding1 '(divide 4 4) '((1)))
(check-eval-to? 5 binding1 '(mult 2 4) '((8)))