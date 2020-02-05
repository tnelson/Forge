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


test {inNexus} for exactly 5 Node expect sat
test {} for 5 Node expect sat
test {~edges != ~(~edges)} for 5 Node expect sat -- FLIP EXPECTATION TO GET PATTERN ERROR 
test {edges != ~(~edges)} for 5 Node expect unsat
test {edges = ^edges} for 5 Node expect sat
test {outNexus and not inNexus} for 5 Node expect sat
test {#Node = 4} for exactly 5 Node expect unsat
test {#Node = 4} for 5 Node expect sat
test {*edges != ^edges + iden} for 5 Node expect unsat
test {edges - (edges.edges + edges.edges.edges) != edges - edges.edges - edges.edges.edges} for 7 Node expect unsat
test {some edges - univ->univ} for 5 Node expect unsat
test {#edges = 0} for 3 Node expect sat
test {lone edges and no edges} for 3 Node expect sat
test {#edges = 1} for 3 Node expect sat
test {lone edges and one edges} for 3 Node expect sat
test {#edges = 2} for 3 Node expect sat
test {#edges = 3} for 3 Node expect sat
test {#edges = 4} for 3 Node expect sat
test {#edges = 5} for 3 Node expect sat
test {#edges = 6} for 3 Node expect sat
test {#edges = 7} for 3 Node expect sat
test {#edges = 8} for 3 Node, 5 Int expect sat
test {#edges = 9} for 3 Node, 5 Int expect sat
test {#edges = 10} for 3 Node, 5 Int expect unsat
test {edges = Node->Node} for 3 Node expect sat
test {edges = iden} for 3 Node expect unsat -- because iden contains ints as well
test {edges = iden & Node->Node} for 3 Node expect sat -- restrict to only iden on Nodes
test {edges = iden & Node->Node and #edges != 3} for exactly 3 Node expect unsat 

