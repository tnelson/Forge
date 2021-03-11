#lang forge/core

(set-verbosity 10)

(sig A)
(sig B)

(run my-run
     #:backend pardinus)
(define result-tree (forge:get-result my-run))
(define result-generator (forge:make-model-generator result-tree 'next))
(for ([i (range 30)])
  (println i)
  (println (result-generator)))
; (display my-run)
