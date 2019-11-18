#lang forge

sig Node {
    edges: Node
}

/*
without:
    problem size: variables = 858, clauses = 1438, state = 35 bits
    solving time (ms): total = 85, parsing = -1, translation = 73, SAT = 13
with:
    problem size: variables = 58, clauses = 63, state = 10 bits
    solving time (ms): total = 45, parsing = -1, translation = 45, SAT = 1
*/
/*$(break edges 'linear 'irref)*/

// this is redundant when edges is declared linear:
pred linear_edges {
    some init: Node { some term: Node {
        edges in (Node-term) -> (Node-init)   // one->one
        all n: (Node-init) { one edges.n }
        all n: (Node-term) { one n.edges }
        init.*edges = Node
    }}
}

query1 : run linear_edges for exactly 5 Node