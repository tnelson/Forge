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

pred isNotRoot[n : Node] {
    isDirectedTree
    some edges.n
}

pred bothRoots[x : Node, y : Node] {
    isRoot[x]
    isRoot[y]
}

pred arethesame [x : Node, y : Node] {
    x = y
}

// Quantified variables do not need to be used 
assert all r1, r2 : Node | isDirectedTree is sufficient for isDirectedTree

// Allow multiple quantifications + check scopes
assert all r : NonNode, r1 : Node | isRoot[r1] is necessary for isDirectedTree for 1 Node, 1 NonNode

// Check explicit bounds with multiple qs, including scopes
assert all r : NonNode, r1 : Node | isRoot[r1] is necessary for isDirectedTree
  for 1 NonNode 
  for {
    Node = `Node0 
    no edges
  }

assert all x : Node | isDirectedTree is necessary for isDirectedTree
assert all r1, r2 : Node | isRoot[r1] is sufficient for isRoot[r1]
assert all r1, r2 : Node | arethesame[r1, r2] is necessary for arethesame[r1, r2]  
assert all r1, r2 : Node | bothRoots[r1, r2] is sufficient for arethesame[r1, r2] 

// Ensure disj works
assert all disj r1, r2 : Node | arethesame[r1, r2] is necessary for arethesame[r1, r2]  for 1 Node
assert all disj r1, r2 : Node | isRoot[r1] is sufficient for isNotRoot[r2] 

// Ensure that these with quantifiers work with test suite 
test suite for isDirectedTree {
    assert all x : Node | isDirectedTree is necessary for isDirectedTree
}
// Ensure that it's OK for the suite to be for a predicate with arguments, which we quantify over
test suite for isRoot {
    assert all r1, r2 : Node | isRoot[r1] is sufficient for isRoot[r1]
}