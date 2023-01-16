#lang forge/core

(set-option! 'verbose 0)

(sig A)
(sig B)
(relation r (A B))

(run sat-run)
(run unsat-run
     #:preds [(some A) (some B) (= A B)])

; Model-generator produces a stream
(define sat-gen (forge:make-model-generator (forge:get-result sat-run) 'next))
(define unsat-gen (forge:make-model-generator (forge:get-result unsat-run) 'next))

(unsat-gen)
(sat-gen)
(unsat-gen)
(sat-gen)
