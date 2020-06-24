#lang forge

sig Node {
    edges: set Node->Color
}

abstract sig Color {}
one sig Red extends Color {}
one sig Green extends Color {}
one sig Blue extends Color {}


inst TestInst {
    Node = N1 + N2 + N3 + N4
    edges = (N1->N2 + N2->N3 + N3->N4 + N4->N1) -> Red0 + -- Cycle

            (Node->Node) -> Green0 + -- Complete

            (N1->(N1 + N2 + N3 + N4) + -- <= relation
             N2->(N2 + N3 + N4) + 
             N3->(N3 + N4) + 
             N4->N4) -> Blue0
}

pred All {
    all n: Node | (edges.Green)[n] = Node
    all n1, n2: Node | (n1->n2 + n2->n1) in edges.Blue implies n1 = n2
    all n: Node, c: Color | n->n in ^(edges.c)
}

pred Some {
    some n: Node | n->Node in edges.Blue
    some n1, n2: Node | n1 != n2 and n1->n2->Color in edges
    some n: Node, c: Color | Node->n->c in edges and n->Node->c !in edges
}

pred No {
    no n: Node | n->Node->Red in edges
    no n1, n2: Node | n1 != n2 and n1 = n2
    no n: Node, c: Color | n = c
}

pred SomePred[n: Node] {
    n->Node->Red in edges
}

pred Equivalences {
    (all n: Node | SomePred[n]) iff
    (not (some n: Node | not SomePred[n]))

    (all n: Node | SomePred[n]) iff
    (no n: Node | not SomePred[n])

    (some n: Node | SomePred[n]) iff
    (not (no n: Node | SomePred[n]))
}

test expect QuantifiedFormulas {
    AllQuant : {not All} for TestInst is unsat
    SomeQuant : {not Some} for TestInst is unsat
    --NoQuant : {not No} for TestInst is unsat -- CURRENTLY BUGGED!

    QuantifierEquivalences : {not Equivalences} is unsat
}
