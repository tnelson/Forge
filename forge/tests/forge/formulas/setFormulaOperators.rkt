#lang forge

option verbose 0

sig Node {
    edges: set Node
}

pred In {
    all n: Node |
        n.edges in Node

    all n: Node |
        Node ni n.edges


    -- reflexive
    all n: Node |
        n.edges in n.edges

    -- anti-symmetric
    all n1, n2: Node |
        (n1.edges in n2.edges and n2.edges in n1.edges) => n1.edges = n2.edges

    -- transitive
    all n1, n2, n3: Node |
        (n1.edges in n2.edges and n2.edges in n3.edges) => n1.edges in n3.edges

    -- ni properly defined
    all n1, n2: Node |
        n1.edges in n2.edges => n2.edges ni n1.edges
}

pred Equals {

    Node = Node

    -- reflexive
    all n: Node |
        n.edges = n.edges

    -- symmetric
    all n1, n2: Node |
        n1.edges = n2.edges => n2.edges = n1.edges

    -- transitive
    all n1, n2, n3: Node |
        (n1.edges = n2.edges and n2.edges = n3.edges) => n1.edges = n3.edges
}

pred ComboNot { -- !=, !in, !ni
    all n1, n2: Node |
        n1.edges !in n2.edges or n1.edges !ni n2.edges implies n1.edges != n2.edges
}

test expect SetFormulaOperators {
    InOps : In is theorem
    EqualsOp : Equals is theorem
    ComboNotOps : ComboNot is theorem
}
