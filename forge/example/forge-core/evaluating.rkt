#lang forge/core

;(set-verbosity 10)

(sig A)
(sig B)

(relation R (A B))

(run my-run)
(define st (forge:Run-result my-run))
(define f (stream-first st))

(evaluate my-run f (in (join A R) B))
(evaluate my-run f (+ A (atom 'B1)))
; (evaluate my-run f (+ (node/int/constant 1) (node/int/constant 2)))

(define s (stream-first (stream-rest st)))

(evaluate my-run s (in (join A R) B))
(evaluate my-run s (+ A B))