#lang forge/core

(set-verbosity 10)

(sig A)
(sig B)
(relation r (A B))

(run my-run
     #:solver TargetSATSolver
     #:backend pardinus)
(is-sat? my-run)
