#lang racket

(require "sigs.rkt")

; there is a sig node, with a field graph of type (set node). (we are not defaulting to one; might in fact later require explicit set/one)
(declare-sig node ((graph node)))

; acyclic graph (side node: Kodkod has acyclic relation pred; is this faster?)
(fact (no (& iden (^ graph))))

; find instance with a path of length 2
(define pathlen2 (some ([n1 node] [n2 node]) (in n1 (join n2 (^ graph)))))

; run command currently has "lower int bound"; anywhere from 5-6 nodes
(run pathlen2 ((node 5 6)))

