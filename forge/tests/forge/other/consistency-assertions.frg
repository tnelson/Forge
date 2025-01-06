#lang forge

option run_sterling off
option verbose 0

sig Node {edges: set Node}

pred p {
	edges.~edges in iden -- Injective, each child has at most one parent
	lone edges.Node - Node.edges -- At most one element that does not have a parent
	no (^edges & iden) -- No loops
	lone Node or Node in edges.Node + Node.edges -- Either one node or every node has either a child or a parent.
}

pred q { no Node }

assert {} is consistent with p

test suite for p {
	assert q is consistent with p
	assert (lone edges) is consistent with p
	assert (lone edges) is consistent with p for 1 Node
	assert (some edges) is inconsistent with p for 1 Node
	assert q is inconsistent with p for exactly 2 Node
}



