#lang forge

sig Node {
    edges : set Node
}


-- Unary operators

pred Tilde {
    ~edges = {n1: Node, n2: Node | n2->n1 in edges}
}

pred Caret {
    #Node < 4 implies {
        ^edges = edges + edges.edges + edges.edges.edges + edges.edges.edges.edges
    }
}

pred Star {
    *edges = ^edges + iden
}


-- Binary operators

pred Plus {
    all n1, n2: Node |
        n1.edges + n2.edges = {n: Node | n in n1.edges or n in n2.edges}
}

pred Minus {
    all n1, n2: Node |
        n1.edges - n2.edges = {n: Node | n in n1.edges and n !in n2.edges}
}

pred Ampersand {
    all n1, n2: Node |
        n1.edges & n2.edges = {n: Node | n in n1.edges and n in n2.edges}
}

pred Arrow {
    all n1, n2: Node |
        n1.edges -> n2.edges = {n3: Node, n4: Node | n3 in n1.edges and n4 in n2.edges}
}

pred Dot {
    edges.edges = {n1: Node, n2: Node | some n3: Node | n3 in n1.edges and n3 in edges.n2}
}



/* CURRENTLY BUGGED?
pred LessColon {
    all n: Node |
        n.edges <: edges = {n1: Node, n2: Node | n1->n2 in edges and n1 in n.edges}
}

pred ColonGreater {
    all n: Node |
        edges :> n.edges = {n1: Node, n2: Node | n1->n2 in edges and n2 in n.edges}
}
*/


test expect ExpressionOperators {
    tilde : {not Tilde} is unsat
    caret : {not Caret} is unsat
    star : {not Star} is unsat
    plus : {not Plus} is unsat
    minus : {not Minus} is unsat
    ampersand : {not Ampersand} is unsat
    arrow : {not Arrow} is unsat
    dot : {not Dot} is unsat
    --lessColon : {not LessColon} is unsat
    --colonGreater : {not ColonGreater} is unsat
}