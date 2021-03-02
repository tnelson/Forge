#lang forge/core

(set-option! 'verbose 0)
(set-option! 'problem_type 'temporal)

(sig Node #:is-var "var")


(inst state0
      (= Node Node0))

(inst state1
      (= Node Node1))

(inst domain
      (in Node (+ Node0 (+ Node1 (+ Node2 Node3)))))

(trace bagel domain 0 state0 state1 state0 state1)

#;(run woah
     #:preds [(some Node)]
     #:bounds bagel)
;(display woah)
