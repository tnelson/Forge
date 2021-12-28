#lang forge/core
(sig Node)
(relation next (Node Node))

(sig A #:one #:extends Node)
(sig B #:one #:extends Node)
(pred arrow (in (-> A B) next))


(run cardi-run #:preds [arrow])