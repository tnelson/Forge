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
    r in edges.Node - Node.edges
}

pred bothRoots[x : Node, y : Node] {
    isRoot[x]
    isRoot[y]
}

pred eq [x : Node, y : Node] {
    x = y
}

assert all r1, r2 : Node | bothRoots[r1, r2] is sufficient for eq[r1, r2]

// Quantifiers not needed
assert all x : Node | isDirectedTree is necessary for isDirectedTree
assert all r1, r2 : Node | isDirectedTree is sufficient for isDirectedTree


// Allow multiple quantifications
assert all r : NonNode, r1, r2 : Node | isRoot[r1] is necessary for isDirectedTree

