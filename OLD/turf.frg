#lang forge

abstract sig Node {
	edges: set Node
}

one sig N0 extends Node {}
one sig N1 extends Node {}

inst jon {
	Node = `n0 + `n1
	--N0 = `n0
	--N1 = `n1
	edges = N0->N1 + N1->N0
}

run {} for jon
