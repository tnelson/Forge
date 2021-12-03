#lang forge

option problem_type temporal
option min_tracelength 4

var sig Node {
	--var edges : set Node,
	--fruit : set Node
}

pred plusPrime {
	some n0, n1 : Node {
		n0 != n1
		Node = n0 + n1
		some n2 : Node' {
			n2 != n0 and n2 != n1
			Node' = n1 + n2
			--Node'' = Node + Node' = n0 + n1 + n2 + n2 = n0 + n1 + n2
			Node'' = (Node + Node')
		}
	}
}

test expect {
	--Time is 0 indexed in this title
	threeNodeInTime2 : {plusPrime implies (#(Node'') = 3)} is theorem
}
