#lang forge/core

(set-option! 'verbose 0)

(sig Node)
(relation edges (Node Node))

(pred stuff
      (in Node (join Node edges)))

(pred fruit
      (= Node (+ (atom 'Node0) (+ (atom 'Node1) (atom 'Node2)))))

#;(run orange
     #:preds [stuff fruit])
;(display orange)
