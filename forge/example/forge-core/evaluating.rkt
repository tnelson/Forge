#lang forge/core

(set-verbosity 10)

(sig A)
(sig B)

(relation R (A B))

(run my-run)
(define st (forge:Run-result my-run))
(define f (stream-first st))

(evaluate my-run f (in (join A R) B))
(evaluate my-run f (+ A (atom 'B1)))
(evaluate my-run f (add (int 1) (int 2)))

(define s (stream-first (stream-rest st)))

(evaluate my-run s (in (join A R) B))
(evaluate my-run s (+ A B))

(println "---------------------------------")

(run my-run2)
(define st2 (forge:Run-result my-run2))
(define f2 (stream-first st2))

(evaluate my-run2 f2 (in (join A R) B))
(evaluate my-run2 f2 (+ A (atom 'B1)))
(evaluate my-run2 f2 (add (int 1) (int 2)))

(define s2 (stream-first (stream-rest st2)))

(evaluate my-run2 s2 (in (join A R) B))
(evaluate my-run2 s2 (+ A B))



