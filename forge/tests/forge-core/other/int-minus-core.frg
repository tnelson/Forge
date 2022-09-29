#lang forge/core
(sig Node)
(relation next (Node Node))
(relation val (Node Int))

(sig A #:one #:extends Node)
(sig B #:one #:extends Node)
(pred intMinus-core (some (- (join A val) (join B val))))


(run cardi-run #:preds [intMinus-core])