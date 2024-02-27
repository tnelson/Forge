#lang forge

option run_sterling off
option verbose 0


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


assert all r1, r2 : Node | isRoot[r1] is necessary for isNotRoot[r2] 

example thisIsNotATree is {isDirectedTree} for {
    Node = `Node1
    edges = `Node1->`Node1
}

test expect {
    t1 : {some r1 : Node | isRoot[r1]} is theorem
}
