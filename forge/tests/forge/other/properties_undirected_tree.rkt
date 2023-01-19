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


// Section 1: Testing Valid, simple underconstraints

 underconstraint isUndirected of isUndirectedTree
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


underconstraint emptyofone of isUndirectedTree
 {
    (no edges)
 } 
 for 1 Node


// Section 2: Testing Valid, simple overconstraints

overconstraint TreeWithEdges of isUndirectedTree
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


// Section 3: Self


overconstraint foo of isUndirectedTree
 {
   isUndirectedTree
 } 


underconstraint bar of isUndirectedTree
 {
   isUndirectedTree
 } 




