#lang forge

/*
  Regression test: appropriate capture of pred-spacers with 1-arg && nodes
*/

----------------------------------------------------------------------------------
option solver MiniSatProver 
option logtranslation 2     
option coregranularity 2    
option core_minimization rce
----------------------------------------------------------------------------------

pred foo {
    some n1, n2, n3: Node | {
        n1->n2 in edges
        n2->n1 in edges 
        n3 not in n3.edges
    }
}

sig Node {edges: set Node}
test expect {
    complexUnsat: {
        foo 
        no edges
    } is unsat
}