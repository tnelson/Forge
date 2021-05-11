#lang forge/core

(set-verbosity 10)

(sig A)
(sig B)

(relation R (A B))

(run my-run)
(define st (forge:Run-result my-run))
(define f (tree:get-value st))

(evaluate my-run f (in (join A R) B))
(evaluate my-run f (+ A (atom 'B1)))
(evaluate my-run f (add (int 1) (int 2)))

(define s (tree:get-value (tree:get-child st 'next)))

(evaluate my-run s (in (join A R) B))
(evaluate my-run s (+ A B))

(println "---------------------------------")

(run my-run2)
(define st2 (forge:Run-result my-run2))
(define f2 (tree:get-value st2))

(evaluate my-run2 f2 (in (join A R) B))
(evaluate my-run2 f2 (+ A (atom 'B1)))
(evaluate my-run2 f2 (add (int 1) (int 2)))

(define s2 (tree:get-value (tree:get-child st2 'next)))

(evaluate my-run2 s2 (in (join A R) B))
(evaluate my-run2 s2 (+ A B))



