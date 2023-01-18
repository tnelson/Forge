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

// This is an overconstraint, not an underconstraint.

underconstraint TreeWithEdges of isUndirectedTree
 {
    edges = ~edges // Symmetric 
    Node->Node in *edges // Connected
    no iden & edges // Irreflexive

    all n : Node | {
        all m : Node | {
            (n->m + m->n) in edges implies (n->m + m->n) not in ^(edges - (n->m + m->n))
        }
    }

    some edges
 } 
 where  { }

 
