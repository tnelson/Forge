#lang forge

sig Node {
  edges: set Node
         }

pred symmetric {
  edges = ~edges
                }

pred noselfloops {
  no iden & edges
                  }

pred nexus {
  some n1: Node | all n2: Node-n1 | n2 in Node.^edges
           }

-- disj
--pred connected {
--  all disj n1, n2: Node | n1 in Node.^edges
--                }

query1 : run {symmetric noselfloops nexus} for 5 Node
