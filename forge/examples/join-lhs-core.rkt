#lang forge/core

(sig Node)
(relation edges (Node Node))

(pred nodeOnLeft
      (in (-> (join Node edges) (join Node edges))
          edges))
(run nl #:preds nodeOnLeft)
(display nl)

#|
(pred edgesOnLeft
      (in (-> (join edges Node) (join edges Node))
          edges))
(run el #:preds edgesOnLeft)
(display el)
|#
