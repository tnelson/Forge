#lang forge/check-ex-spec/core "sample"

(sig Node)
(relation edges (Node Node))

(example EG hasSink
  (= Node (+ N1 (+ N2 N3)))
  (= edges (-> Node N2)))

(example NEG (not hasSink)
  (= Node (+ N1 (+ N2 N3)))
  (= edges (+ (-> (+ N1 N3) N2)
              (-> N2 N3))))