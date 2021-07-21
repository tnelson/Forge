#lang forge

sig Node {
	edges : set Node
}

pred nodeOnLeft {
	Node.edges->Node.edges in edges
}

run {
	nodeOnLeft
}

pred edgesOnLeft {
	edges.Node->edges.Node in edges
}

run {
	edgesOnLeft
}
