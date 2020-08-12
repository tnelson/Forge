#lang forge/core

(set-verbosity 10)

(sig A)
(sig B)
(relation r (A B))

(run my-run
     #:backend pardinus)
(stream-first (forge:get-result my-run))

(evaluate my-run #f (join A r))
(evaluate my-run #f (join (atom 'A3) r))
(evaluate my-run #f (+ (atom 'B0) (join r (atom 'B0))))
