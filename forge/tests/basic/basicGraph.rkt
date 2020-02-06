#lang forge

-- VERSION FOR AUTOMATED TESTING

sig Node {
  edges: set Node
}

pred outNexus {
  some n: Node | all n2: Node | n->n2 in edges
}

pred inNexus {
  all n: Node | some n2: Node | n->n2 in edges
}


test expect {
 {inNexus} for exactly 5 Node is sat
 {} for 5 Node is sat
 {~edges != ~(~edges)} for 5 Node is sat -- FLIP EXPECTATION TO GET PATTERN ERROR 
 {edges != ~(~edges)} for 5 Node is unsat
 {edges = ^edges} for 5 Node is sat
 {outNexus and not inNexus} for 5 Node is sat
 {*edges != ^edges + iden} for 5 Node is unsat
 {edges - (edges.edges + edges.edges.edges) != edges - edges.edges - edges.edges.edges} for 7 Node is unsat
 {some edges - univ->univ} for 5 Node is unsat
 {#edges = 0} for 3 Node is sat
 {lone edges and no edges} for 3 Node is sat
 {#edges = 1} for 3 Node is sat
 {lone edges and one edges} for 3 Node is sat
 {edges = Node->Node} for 3 Node is sat
 {edges = iden} for 3 Node is unsat -- because iden contains ints as well
 {edges = iden & Node->Node} for 3 Node is sat -- restrict to only iden on Nodes
 {edges = iden & Node->Node and #edges != 3} for exactly 3 Node is unsat
}

test expect node_cardinality {
 {#Node = 4} for exactly 5 Node is unsat
 {#Node = 4} for 5 Node is sat
}

test expect edges_cardinality {
 {#edges = 2} for 3 Node is sat
 {#edges = 3} for 3 Node is sat
 {#edges = 4} for 3 Node is sat
 {#edges = 5} for 3 Node is sat
 {#edges = 6} for 3 Node is sat
 {#edges = 7} for 3 Node is sat
 {#edges = 8} for 3 Node, 5 Int is sat
 {#edges = 9} for 3 Node, 5 Int is sat
 {#edges = 10} for 3 Node, 5 Int is unsat
} 

