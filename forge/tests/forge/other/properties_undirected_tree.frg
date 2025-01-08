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


assert {} is necessary for isUndirectedTree

//assert {all m, n : Node | n->m in edges implies m->n in edges} is necessary for isUndirectedTree


pred isUndirected
{
     all m, n : Node | n->m in edges implies m->n in edges
} 


test suite for isUndirected {

    test expect {
        {isUndirected} is sat
    }

    test expect {
        {one Node} is sat // This should result in a warning but not an error.
    }

    example line is {not isUndirected} for {
        Node = `Node0 + `Node1 + `Node2
        edges = `Node0->`Node1 + `Node1->`Node2 
    }

    example empty is {isUndirected} for {
        no Node 
        no edges
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


pred edgeless  {
    (no edges)
 } 

pred treeWithEdges 
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




test suite for isUndirectedTree {
    
    assert isUndirected is necessary for isUndirectedTree
    assert edgeless is necessary for isUndirectedTree for 1 Node

    assert treeWithEdges is sufficient for isUndirectedTree

    assert isUndirectedTree is necessary for isUndirectedTree
    assert isUndirectedTree is sufficient for isUndirectedTree

}




