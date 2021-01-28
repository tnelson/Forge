#lang forge/core

(sig a)
(run my-run)

(define res (forge:get-result my-run))

(stream-first res)

(forge:close-run my-run)

(stream-first (stream-rest res))