#lang forge
option verbose 0

-- Regression test:
-- make sure special handling for integers in bounds allows for use in `inst` blocks.

sig Node {
  edges: set Int -> Node
}

inst optimize {
  Node = `Node0 + `Node1 + `Node2 + `Node3
  edges in Node -> (0 + 1) -> Node
}

test expect {
    boundsNonEmpty: {some edges} for 5 for optimize is sat
    boundsLimit1: { some n1, n2: Node | sum[n1.edges.n2] > 1} for 5 for optimize is unsat
    boundsLimit2: { some n1, n2: Node | sum[n1.edges.n2] < 0} for 5 for optimize is unsat
}
