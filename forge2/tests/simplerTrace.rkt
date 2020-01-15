#lang forge

sig Node {
    edges: Node
}

/*(declare-sig Node (
    (edges Node)
))*/



/* stats are for exactly 5 Node * from least to most strict */

// fact edges: default
/*  problem size: variables = 858, clauses = 1438, state = 35 bits
    solving time (ms): total = 85, parsing = -1, translation = 73, SAT = 13 */

// fact edges: irref
/*  problem size: variables = 630, clauses = 1026, state = 30 bits
    solving time (ms): total = 76, parsing = -1, translation = 66, SAT = 11 */

// fact edges: acyclic
/*  problem size: variables = 164, clauses = 236, state = 20 bits
    solving time (ms): total = 46, parsing = -1, translation = 45, SAT = 2 */

// fact edges: tree
/*  problem size: variables = 185, clauses = 268, state = 25 bits
    solving time (ms): total = 52, parsing = -1, translation = 51, SAT = 2 */

fact edges: linear
/*  problem size: variables = 58, clauses = 63, state = 10 bits
    solving time (ms): total = 40, parsing = -1, translation = 40, SAT = 1 */




// this is redundant when edges is declared linear:
pred linear_edges {
    /*some init: Node { some term: Node {
        edges in (Node-term) -> (Node-init)   // one->one
        all n: (Node-init) { one edges.n }
        all n: (Node-term) { one n.edges }
        init.*edges = Node
    }}*/
    some Node
}

query1 : run linear_edges for exactly 5 Node

