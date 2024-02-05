#lang forge

sig NonNode {}
sig Node {edges: set Node}

pred isDirectedTree {
	edges.~edges in iden -- Injective, each child has at most one parent
	lone edges.Node - Node.edges -- At most one element that does not have a parent
	no (^edges & iden) -- No loops
	lone Node or Node in edges.Node + Node.edges -- Either one node or every node has either a child or a parent.
}

pred isRoot[r : Node] {
	isDirectedTree
    one r
    (some edges) => (r in edges.Node - Node.edges)
}

pred bothRoots[x : Node, y : Node] {
    isRoot[x]
    isRoot[y]
}

pred arethesame [x : Node, y : Node] {
    x = y
}

/*
// Quantifiers not needed

assert all r1, r2 : Node | isDirectedTree is sufficient for isDirectedTree


// Allow multiple quantifications
assert all r : NonNode, r1 : Node | isRoot[r1] is necessary for isDirectedTree for 1 Node

test expect {

    t1 : {all r1, r2 : Node | arethesame[r1, r2] implies arethesame[r1, r2]  } is theorem
}
*/


// This does not pass
//assert all x : Node | isDirectedTree is necessary for isDirectedTree
//assert all r1, r2 : Node | isRoot[r1] is sufficient for isRoot[r1]
assert all r1, r2 : Node | arethesame[r1, r2] is sufficient for arethesame[r1, r2]  