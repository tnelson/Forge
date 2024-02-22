#lang forge

option run_sterling off

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
    isDirectedTree
}

// Should pass
assert all r1, r2 : Node | arethesame[r1, r2]  is necessary for bothRoots[r1, r2] 
//Should fail
assert all r1, r2 : Node | arethesame[r1, r2]  is sufficient for bothRoots[r1, r2]


