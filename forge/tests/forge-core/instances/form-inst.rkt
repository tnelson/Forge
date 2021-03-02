#lang forge/core

(set-option! 'problem_type 'temporal)
(set-option! 'verbose 0)

(sig Node #:is-var "var")
(relation edges (Node Node) #:is-var "var")

(inst domain
      (in Node (+ Node0 Node1)))

(inst state0
      (no Node))

(inst state1
      (= Node Node0)
      (= edges (-> Node0 Node0)))

(inst state2
      (= Node (+ Node0 Node1))
      (= edges (+ (-> Node0 Node1)
                  (-> Node1 Node0))))

(trace okay domain 0 state0 state1 state2)

#;(run happy
     #:bounds [okay])
;(display happy)

