#lang forge/core

(set-option! 'problem_type 'temporal)
(set-option! 'verbose 0)

(sig Node #:is-var "var")
(relation edges (Node Node) #:is-var "var")

(inst domain
      (in Node (+ Node0 Node1)))

(form-inst state0
           (no Node))

(form-inst state1
           (= Node (atom 'Node0))
           (= edges (-> (atom 'Node0) (atom 'Node0))))

(form-inst state2
           (= Node (+ (atom 'Node0) (atom 'Node1)))
           (= edges (+ (-> (atom 'Node0) (atom 'Node1))
                       (-> (atom 'Node1) (atom 'Node0)))))

(trace okay 1 state0 state1 state2)

#;(run happy
     #:bounds [domain]
     #:trace okay)
;(display happy)
