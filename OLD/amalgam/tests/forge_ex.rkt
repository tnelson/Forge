#lang forge

sig Node {edges: set Node}

pred isUndirectedTree {
    (Node->Node) in *(edges + ~edges) 
    edges in ~edges
    not (some iden & edges)
    all n1, n2: Node | (n1 in n2.edges implies n1 not in n2.^(edges - (n2->n1)))
}

-- Now defined in forge_ex_test.rkt
--udt: run isUndirectedTree for 7 Node 