#lang forge/core
(sig Node)
(relation next (Node Node))

(sig A #:one #:extends Node)
(sig B #:one #:extends Node)
(pred joincore (some (join A next)))


(run cardi-run #:preds [joincore])