#lang forge

sig Node {
    edges: Node
}

/*$(linear edges)*/

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