#lang forge

sig Node {
    edges: set Color->Node
}

abstract sig Color {}
one sig Red extends Color {}
one sig Green extends Color {}
one sig Blue extends Color {}


-- (Color) -> (Node Node)
fun getGraph[color: set Color]: set Node -> Node {
    {n1: Node, n2: Node |
        some cx: color |
            n1 -> cx -> n2 in edges
    }
}


-- Check two different definitions of disjoint,
-- meaning all color graphs are pairwise disjoint
pred disjointColors {
    no c1: Color, c2: Color - c1 |
        some getGraph[c1] & getGraph[c2]
}


pred disjointColors2 {
    all c1, c2: Color |
        c1 != c2 implies
        no getGraph[c1] & getGraph[c2]
}

equivalentDisjoint : run { -- expect unsat
    not (disjointColors iff disjointColors2)
}


-- Find a partition of the complete graph into the colors
pred coversGraph {
    Node->Node = getGraph[Color]
}

partitionEdges : run {
    coversGraph
    disjointColors
} for exactly 2 Node


-- Find 3 spanning trees that share no edges
pred spanning[graph: set Node->Node] {
    *(graph + ~graph) ni Node->Node
}

pred spanningTree[color: set Color] {
    -- let not implemented yet
    spanning[getGraph[color]]
    no iden & ^(getGraph[color])
    all n1: Node, n2: n1.(getGraph[color]) |
        not spanning[getGraph[color] - n1->n2]
}

pred distinctSpanningTrees {
    disjointColors
    all c: Color |
        spanningTree[c]
}

dst : run {
    distinctSpanningTrees
}

-- Missing functionality: instances are buggy oops

--inst dstInst {
--    Node = Node1 + Node2 + Node3 + Node4
--    Red = Red0
--    edges ni Node1->Red->Node2 +
--             Node1->Red->Node3 +
--             Node2->Red->Node4 +
--             Node3->Red->Node4
--}

--dstCheck : run { -- expect unsat
--    distinctSpanningTrees
--} for dstInst



