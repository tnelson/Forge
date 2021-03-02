#lang forge/core

(set-option! 'verbose 0)
(set-option! 'problem_type 'temporal)

(sig Node #:is-var "var")

(inst domain
      (in Node (+ Node0 (+ Node1 Node2))))

(inst state0
      (no Node))

(inst state1
      (= Node (+ Node0 Node1)))

(inst state2
      (= Node Node2))

(trace order domain 2 state0 state1 state2)

#;(run no-bounds
     #:preds [(and (some Node) (some (prime Node)))])
#;(run trace-only
     #:bounds order)
#;(run without-trace
     #:bounds [domain state0 state1 state2])
#;(run trace-first
     #:bounds [order state0 (no (prime Node))])
#;(run trace-middle
     #:bounds [(some (prime (prime Node))) domain order state2])
#;(run trace-last
     #:bounds [domain state0 state2 order])
