#lang forge/core

(set-option! 'problem_type 'temporal)
(set-option! 'verbose 0)

(sig Node #:is-var "var")

(inst domain
      (in Node (+ Node0 (+ Node1 (+ Node2 (+ Node3 (+ Node4 Node5)))))))

(inst state0
      (= Node (+ Node0 Node1)))

(inst state1
      (= Node (+ Node0 (+ Node1 Node2))))

(inst state2
      (= Node (+ Node0 (+ Node1 (+ Node2 (+ Node3 (+ Node4 Node5)))))))

(inst state3
      state1)

(inst state4
      state0)

(trace some-reps domain 3 state0 state1 state2 state3 state4)

(run no-preds
     #:bounds domain
     #:trace some-reps)
(display no-preds)
