#lang forge/core

(set-verbosity 10)

(sig A)
(sig B)

(run my-run
     #:backend pardinus)
(define result (forge:get-result my-run))
(for ([i (range 30)])
  (println i)
  (println (stream-ref result i)))
; (display my-run)
