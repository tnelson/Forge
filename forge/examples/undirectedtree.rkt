#lang forge

(declare-sig vertex ((edges vertex)))
(fact (= edges (~ edges)))
(fact (in (-> vertex vertex) (* edges)))
(fact (no (& iden edges)))
(fact (all ([v1 vertex] [v2 vertex])
                   (=> (in (+ (-> v1 v2) (-> v2 v1)) edges) (! (in (+ (-> v1 v2) (-> v2 v1)) (^ (- edges (+ (-> v1 v2) (-> v2 v1)))))))))
(run "Undirected Tree" ((vertex 0 5)))