#lang forge/core

(set-verbosity 0)

(sig Node)
(sig Root #:one #:extends Node)

(relation edges (Node Node))

(pred acyclic (no (& iden (^ edges))))
(pred root-connected (in Node (join Root (* edges))))

(run rooted-acyclic-run #:preds [acyclic root-connected] #:bounds ([Node 4 6]))
;(display rooted-acyclic-run)
