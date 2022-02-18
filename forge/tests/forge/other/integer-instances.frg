#lang forge
option verbose 0

-- Regression test:
-- make sure special handling for integers in bounds allows for use in `inst` blocks.
-- make sure this works in both explicit and in implicit inst blocks

sig Node {
  edges: set Int -> Node
}

inst optimize1 {
  Node = `Node0 + `Node1 + `Node2 + `Node3
  edges in Node -> (0 + 1) -> Node
}

inst optimize2 {
  Node = `Node0 + `Node1 + `Node2 + `Node3
  edges ni Node -> (0 + 1) -> Node
}

inst optimize3 {
  Node = `Node0 + `Node1 + `Node2 + `Node3
  edges = Node -> (0 + 1) -> Node
}


test expect {
    in_boundsNonEmpty: {some edges} for 5 for optimize1 is sat
    in_boundsLimit1: { some n1, n2: Node | sum[n1.edges.n2] > 1} for 5 for optimize1 is unsat
    in_boundsLimit2: { some n1, n2: Node | sum[n1.edges.n2] < 0} for 5 for optimize1 is unsat

    ni_boundsNonSingle: {one edges} for 5 for optimize2 is unsat
    ni_boundsLimit1: { some n1, n2: Node | sum[n1.edges.n2] > 1} for 5 for optimize2 is sat
    ni_boundsLimit2: { some n1, n2: Node | sum[n1.edges.n2] < 0} for 5 for optimize2 is sat

    eq_boundsNonSingle: {one edges} for 5 for optimize3 is unsat
    eq_boundsLimit1: { some n1, n2: Node | sum[n1.edges.n2] > 1} for 5 for optimize3 is unsat
    eq_boundsLimit2: { some n1, n2: Node | sum[n1.edges.n2] < 0} for 5 for optimize3 is unsat

    -- Spot issues with implicit conversion
    eq_boundsNonSingleImplicit: {one edges} for 5 for {
      Node = `Node0 + `Node1 + `Node2 + `Node3
      edges = Node -> (0 + 1) -> Node
    } is unsat
    cardinalityImplicit: {no Node} for 3 for {
      #Node = 2      
    } is unsat

}
