#lang forge

option verbose 0

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
    tilde : Tilde is theorem
    caret : Caret is theorem
    star : Star is theorem
    plus : Plus is theorem
    minus : Minus is theorem
    ampersand : Ampersand is theorem
    arrow : Arrow is theorem
    dot : Dot is theorem
    --lessColon : LessColon is theorem
    --colonGreater : ColonGreater is theorem
}
