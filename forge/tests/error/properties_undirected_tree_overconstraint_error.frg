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

pred  isUndirected {
     all m, n : Node | n->m in edges implies m->n in edges
 } 


assert isUndirected is sufficient for isUndirectedTree

 


 
