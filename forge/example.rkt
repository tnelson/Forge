#lang racket

(require "sigs.rkt")
(set-verbosity 10)

(sig Node)
(sig Root #:one #:extends Node)

(relation edges (Node Node))

(pred acyclic (no (& iden (^ edges))))
(pred root-connected (= Node (join Root (* edges))))

(run rooted-dag (acyclic root-connected) ([Node 0 5]))