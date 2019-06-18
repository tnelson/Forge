sig Node {
	edges: set Node,
	tc: set Node,
	gen: set Node
}

fact {
	tc = ^edges
	
	all x, y: Node | {
		((x->y) in gen) iff some n0, n1, n2, n3, n4, n5: Node | {
			((x->n0) in edges or x = n0)
			((n0->n1) in edges or n0 = n1)
			((n1->n2) in edges or n1 = n2)
			((n2->n3) in edges or n2 = n3)
			((n3->n4) in edges or n3 = n4)
			((n4->n5) in edges or n4 = n5)
			((n5->y) in edges)
		}
	}	
}

assert isTC {
	tc = gen
}

check isTC for 6
