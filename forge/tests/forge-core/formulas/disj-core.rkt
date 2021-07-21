#lang forge/core

(sig Node)
(relation edges)

(pred someDisj
      (some ([n1 Node]
             [n2 Node])
        ...))

(pred allDisj
      (all ([n1 Node]
            [n2 Node])
        ...))
