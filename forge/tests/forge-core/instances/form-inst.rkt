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
      (= Node (atom 'Node0))
      (= edges (-> (atom 'Node0) (atom 'Node0))))

(inst state2
      (= Node (+ (atom 'Node0) (atom 'Node1)))
      (= edges (+ (-> (atom 'Node0) (atom 'Node1))
                  (-> (atom 'Node1) (atom 'Node0)))))

(trace okay domain 0 state0 state1 state2)

(run happy
     #:bounds [domain]
     #:trace okay)
(stream-ref (forge:get-result happy) 0)
;(display happy)
