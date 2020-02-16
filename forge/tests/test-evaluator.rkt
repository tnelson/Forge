#lang racket

(require rackunit)
(require "../server/eval-model.rkt")

(define binding1 (make-hash '(
                              (Node . ((Node0)
                                       (Node1)
                                       (Node2)))
                              (edges . ((Node0 Node1)
                                        (Node1 Node2)))
                              )))

(check-eq? (length (eval-exp 'edges binding1 4)) 2)
(check-eq? (length (eval-exp 'Node binding1 4)) 3)

(let ([e '(^ edges)])
  (check-eq? (length (eval-exp e binding1 4)) 3))
(let ([e '(* edges)]) ; the 3 above plus 3 identity tuples
  (check-eq? (length (eval-exp e binding1 4)) 6))

(let ([e '(~ edges)])
  (check-pred (lambda (x) (and (member '(Node2 Node1) x)
                               (member '(Node1 Node0) x)
                               (eq? (length x) 2)))
              (eval-exp e binding1 4)))

