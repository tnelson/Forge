#lang forge
option run_sterling off
option verbose 0

sig NonNode {}
sig Node {edges: set Node}

pred isDirectedTree {
	edges.~edges in iden -- Injective, each child has at most one parent
	lone edges.Node - Node.edges -- At most one element that does not have a parent
	no (^edges & iden) -- No loops
	lone Node or Node in edges.Node + Node.edges -- Either one node or every node has either a child or a parent.
}

pred impossible {
    isDirectedTree
    not isDirectedTree
}

pred producesErr {
    Node in Node->Node
}

assert isDirectedTree is sat
assert impossible is unsat

assert { isDirectedTree and !isDirectedTree } is unsat
assert {} is sat


stderr: assert producesErr is forge_error

-- Test in suite context
test suite for isDirectedTree { assert isDirectedTree is sat for 3 Node }
test suite for impossible { assert impossible is unsat for exactly 1 Node }
test suite for producesErr { assert producesErr is forge_error for exactly 1 Node }
