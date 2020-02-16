#lang forge

-- VERSION FOR AUTOMATED TESTING

-- Test the option command to switch to core-generating solver
option solver MiniSatProver
option verbosity 0

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
 first : {inNexus} for exactly 5 Node is sat
 trivial: {} for 5 Node is sat
 transpose1: {~edges != ~(~edges)} for 5 Node is sat 
 transpose2: {edges != ~(~edges)} for 5 Node is unsat
 tcall: {edges = ^edges} for 5 Node is sat
 preds: {outNexus and not inNexus} for 5 Node is sat
 rtc: {*edges != ^edges + iden} for 5 Node is unsat
 dotminus : {edges - (edges.edges + edges.edges.edges) != edges - edges.edges - edges.edges.edges} for 7 Node is unsat
 testuniv: {some edges - univ->univ} for 5 Node is unsat
 testcardzero: {#edges = 0} for 3 Node is sat
 testloneno: {lone edges and no edges} for 3 Node is sat
 testcardone: {#edges = 1} for 3 Node is sat
 testloneone: {lone edges and one edges} for 3 Node is sat
 {edges = Node->Node} for 3 Node is sat
 {edges = iden} for 3 Node is unsat -- because iden contains ints as well
 {edges = iden & Node->Node} for 3 Node is sat -- restrict to only iden on Nodes
 {edges = iden & Node->Node and #edges != 3} for exactly 3 Node is unsat
 testzeroscope1: {#Node = 0} for 0 Node is sat
 testzeroscope2: {#Node = 1} for 0 Node is unsat
 testboxjoin: {edges[Node] in Node} is sat
 testboxjoinnone: {some edges[none]} is unsat 
}

test expect ifte {
    ifte1: {#Node > 1 => some edges else no edges} for exactly 2 Node is sat
    ifte2: {some edges and (#Node > 1 => some edges else no edges)} for exactly 1 Node is unsat
}

test expect comprehension {
    comp1: { some {n: Node | some n.edges} } is sat
    comp2: { some {n: Node, m: Node-n | n->m in edges} } is sat
    comp3: { some {n: Node, m: univ-Node | n->m in edges} } is unsat
    comp4: { {n: Node, m: Node | n->m in edges} != edges} is unsat -- ordering    
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

