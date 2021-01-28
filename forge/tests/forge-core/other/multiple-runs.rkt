#lang forge/core

(set-option! 'verbose 0)

(sig A)
(sig B)
(relation r (A B))

(run sat-run)
(run unsat-run
     #:preds [(some A) (some B) (= A B)])

(define sat-gen (forge:make-model-generator (forge:get-result sat-run)))
(define unsat-gen (forge:make-model-generator (forge:get-result unsat-run)))

(unsat-gen)
(sat-gen)
(unsat-gen)
(sat-gen)
