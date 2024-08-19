#lang forge

option run_sterling off

option verbose 0
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
    --Time is 0 indexed in this test name
    threeNodeInTime2 : {plusPrime implies (#(Node'') = 3)} is theorem

    -- Regression tests
    noError_comprehension: { some {n: Node | some n}' } is sat
    noError_ite: { some (some Node => Node else Node)' } is sat
    -- `univ` and `iden` may vary over time in the presence of var sigs.
    noError_univ_when_var_sig: {some univ'} is sat
    noError_iden_when_var_sig: {some iden'} is sat
    -- `none` won't vary over time, even if some sigs are var.
    error_none_when_var_sig: {some none'} is forge_error : "non-temporal context"
   
}
