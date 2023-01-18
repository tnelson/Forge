#lang forge
option run_sterling off

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

// This is an underconstraint, not an overconstraint.

 overconstraint isUndirected of isUndirectedTree
 {
     all m, n : Node | n->m in edges implies m->n in edges
 } 
 where { }


 
