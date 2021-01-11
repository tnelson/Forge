#lang forge/core

(sig Node)
(relation edges (Node Node))

(pred (isSource n)
  (in (- Node n)
      (join n edges)))

(pred hasSink
  (some ([n Node])
    (= Node (join edges n))))
