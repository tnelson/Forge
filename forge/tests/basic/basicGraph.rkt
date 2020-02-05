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




