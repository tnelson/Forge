#lang forge/check-ex-spec/core "undirected-tree"

(inst basic-example
  (= Node (+ Node1 (+ Node2 (+ Node3 Node4))))

  (= edges (+ (-> Node1 (+ Node2 (+ Node3 Node4))) 
              (~ (-> Node1 (+ Node2 (+ Node3 Node4)))))))

(test inst-is-valid
      #:preds [(undirectedTree edges)]
      #:bounds [basic-example]
      #:expect sat)

(example inst-is-valid2 
         (undirectedTree edges) 
         [basic-example])

(example inst-is-valid3
         (undirectedTree edges)
         [(= Node (+ Node1 (+ Node2 (+ Node3 Node4))))

          (= edges (+ (-> Node1 (+ Node2 (+ Node3 Node4))) 
                      (~ (-> Node1 (+ Node2 (+ Node3 Node4))))))])

(test Node->Node-invalid
      #:preds [(undirectedTree (-> Node Node))]
      #:bounds [basic-example]
      #:expect unsat)

(example Node->Node-invalid2 
         (not (undirectedTree (-> Node Node))) 
         [basic-example])
