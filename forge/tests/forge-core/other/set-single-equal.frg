#lang forge/core
(sig Node)
(relation next (Node Node))

(sig A #:one #:extends Node)
(sig B #:one #:extends Node)
(pred set-single-equal-core (= (join A next) Node))


(run cardi-run #:preds [set-single-equal-core])