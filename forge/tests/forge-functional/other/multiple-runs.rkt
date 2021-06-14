#lang forge/core

(set-option! 'verbose 0)

(define A (make-sig 'A))
(define B (make-sig 'B))
(define r (make-relation 'r (list A B)))

(define sat-run (make-run #:name 'sat-run #:sigs (list A B) #:relations (list r)))
(define unsat-run (make-run #:name 'unsat-run
                            #:preds (list (some A) (some B) (= A B))
                            #:sigs (list A B)
                            #:relations (list r)))

; Model-generator produces a stream
(define sat-gen (forge:make-model-generator (forge:get-result sat-run) 'next))
(define unsat-gen (forge:make-model-generator (forge:get-result unsat-run) 'next))

(unsat-gen)
(sat-gen)
(unsat-gen)
(sat-gen)
