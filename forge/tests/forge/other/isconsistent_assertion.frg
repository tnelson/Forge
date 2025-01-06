#lang forge

// option run_sterling off
// option verbose 0

//sig NonNode {}
//sig Node {edges: set Node}

pred p {
	//edges.~edges in iden -- Injective, each child has at most one parent
	//lone edges.Node - Node.edges -- At most one element that does not have a parent
	//no (^edges & iden) -- No loops
	//lone Node or Node in edges.Node + Node.edges -- Either one node or every node has either a child or a parent.
}

pred q{}


assert {} is consistent with p

