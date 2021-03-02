#lang forge/core

(set-option! 'verbose 0)
(set-option! 'problem_type 'temporal)

(sig Node #:is-var "var")


(form-inst state0
           (= Node (atom 'Node0)))

(form-inst state1
           (= Node (atom 'Node1)))

(inst domain
      (in Node (+ Node0 (+ Node1 (+ Node2 Node3)))))

(trace bagel 0 state0 state1 state0 state1)

#;(run woah
     #:preds [(some Node)]
     #:bounds domain
     #:trace bagel)
;(display woah)
