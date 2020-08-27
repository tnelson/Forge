#lang forge/check-ex-spec/core "undirected-tree"

(inst basic-example
  (= Node (+ Node1 (+ Node2 (+ Node3 Node4))))

  (= edges (+ (-> Node1 (+ Node2 (+ Node3 Node4))) 
              (~ (-> Node1 (+ Node2 (+ Node3 Node4)))))))

(test inst-is-valid
      #:preds [(undirected-tree edges)]
      #:bounds [basic-example]
      sat)

(test Node->Node-invalid
      #:preds [(undirected-tree (-> Node Node))]
      #:bounds [basic-example]
      unsat)