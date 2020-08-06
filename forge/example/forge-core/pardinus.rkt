#lang forge/core

;(set-verbosity 10)

(sig A)
(sig B)
(relation r (A B))

(run my-run)

(define result (forge:get-result my-run))
(display my-run)
(define t (stream-ref result 5))

(run my-run2
     #:target t
     #:solver TargetSATSolver
     #:backend pardinus)
(display my-run2)

