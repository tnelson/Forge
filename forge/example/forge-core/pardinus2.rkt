#lang forge/core

(set-verbosity 10)

(sig A)
(sig B)
(relation r (A B))

(run my-run
     #:backend pardinus)
(stream-first (forge:get-result my-run))
(evaluate my-run #f (join A r))
