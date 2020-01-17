#lang forge

sig Node {
  edges: Node
         }

pred symmetric {
  edges = ~edges
                }

pred noselfloops {
  no iden & edges
                  }

pred nexus {
  some n: Node | all n': Node-n | n' in n.^edges
           }

-- disj
--pred connected {
--  all disj n1, n2: Node | n1 in n2.^edges
--                }

query1 : run {symmetric noselfloops nexus} for 5 Node