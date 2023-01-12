#lang forge

sig Node {edges: set Node}

pred isUndirectedTree {
    edges = ~edges
    Node->Node in *edges
    no iden & edges
    all n : Node | {
        all m : Node | {
            (n->m + m->n) in edges implies (n->m + m->n) not in ^(edges - (n->m + m->n))
        }
    }
}



 assert isUndirected 
 {
     all m, n : Node | n->m in edges implies m->n in edges
 } 
 is necessary for isUndirectedTree
 where {


        test expect {

            {isUndirected} is sat
        }

        example line is {not isUndirected} for {
            Node = `Node0 + `Node1 + `Node2
            edges = `Node0->`Node1 + `Node1->`Node2 
        }

        example empty is {isUndirected} for {
            Node = none
            edges = none->none
        }

        example refl is {isUndirected} for {
            Node = `Node0
            edges = `Node0->`Node0
        }

        example disconnected is {isUndirected} for {
            Node = `Node0 + `Node1
            edges = `Node0->`Node0
        }
}





assert emptyofone
 {
    (no edges)
 } is sufficient for isUndirectedTree
 for 1 Node

