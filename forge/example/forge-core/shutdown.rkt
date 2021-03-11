#lang forge/core

(sig a)
(run my-run)

(define res (forge:get-result my-run))

(tree:get-value res)

(forge:close-run my-run)

(tree:get-value (tree:get-child res 'next))