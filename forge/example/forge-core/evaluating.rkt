#lang forge/core

;(set-verbosity 10)

(sig A)
(sig B)

(relation R (A B))

(run my-run)
(define s (forge:Run-result my-run))
(define f (stream-first s))

(evaluate my-run f (in (join A R) B))
(evaluate my-run f (join A R))
;(evaluate my-run f (+ 1 2))
