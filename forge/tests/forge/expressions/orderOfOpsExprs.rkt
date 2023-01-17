#lang forge

option run_sterling off


option verbose 0
option problem_type temporal

var sig Node {
	var edges : set Node,
	var fruit : set Node
}

test expect {
	tildeTighterThanPrimeSat : { always ~edges' = (~edges)' } is sat
	tildeTighterThanPrimeTheorem : { always ~edges' = (~edges)' } is theorem
	tranisitiveClosureTighterThanPrimeSat : {
		always ^edges' = (^edges)'
	} is sat
	tranisitiveClosureTighterThanPrimeTheorem : {
		always ^edges' = (^edges)'
	} is theorem
	reflexiveTranisitiveClosureTighterThanPrimeSat : {
		always *edges' = (*edges)'
	} is sat
	reflexiveTranisitiveClosureTighterThanPrimeTheorem : {
		always *edges' = (*edges)'
	} is theorem
}

test expect {
	primeTighterThanLeftJoinSat : {
		always Node.edges' = Node.(edges')
	} is sat
	primeTighterThanLeftJoinTheorem : {
		always Node.edges' = Node.(edges')
	} is theorem
	primeTighterThanRightJoinSat : {
		always edges'.Node = (edges').Node
	} is sat
	primeTighterThanRightJoinTheorem : {
		always edges'.Node = (edges').Node
	} is theorem
}

test expect {
	dotJoinTighterThanBoxJoinIsSat : {
		always Node.edges[fruit] = (Node.edges)[fruit]
	} is sat
	dotJoinTighterThanBoxJoinIsTheorem : {
		always Node.edges[fruit] = (Node.edges)[fruit]
	} is theorem
}

--The next tightest operators are <: and :> but we don't have those in Forge yet

test expect {
	boxTighterThanArrowSat : {
		always (Node.fruit)->edges[Node] = (Node.fruit)->(edges[Node])
	} is sat
	boxTighterThanArrowTheorem : {
		always (Node.fruit)->edges[Node] = (Node.fruit)->(edges[Node])
	} is theorem
}

test expect {
	arrowTighterThanIntersectionSat : {
		always (
			(Node->(Node.fruit) & (edges.Node)->Node) = ((Node->(Node.fruit)) & ((edges.Node)->Node))
		)
	} is sat
	arrowTighterThanIntersectionTheorem : {
		always (
			(Node->(Node.fruit) & (edges.Node)->Node) = ((Node->(Node.fruit)) & ((edges.Node)->Node))
		)
	} is theorem
}

--The next tightest operator is ++ but we don't have it in Forge yet

test expect {
	intersectionTighterThanCardinalitySat : {
		always #Node & Node.edges = #(Node & Node.edges)
	} is sat
	intersectionTighterThanCardinalityTheorem : {
		always #Node & Node.edges = #(Node & Node.edges)
	} is theorem
}

-- cardinality needs to be tighter than union, so
-- #Node.fruit + #Node.edges should become #(Node.fruit) + #(Node.edges)
-- however, since + is union, not addition, this will throw an error
-- there currently is no way to test in forge surface if something raises an error
-- so instead we will add a forge/core test to check this
-- and to check oOps we will see that intersection is tighter than union

test expect {
	intersectionTighterThanUnionSat : {
		always Node.edges & fruit.Node + edges.Node & Node.fruit = (Node.edges & fruit.Node) + (edges.Node & Node.fruit)
	} is sat
	intersectionTighterThanUnionTheorem : {
		always Node.edges & fruit.Node + edges.Node & Node.fruit = (Node.edges & fruit.Node) + (edges.Node & Node.fruit)
	} is theorem
}

test expect {
	unionTighterThanNoSat : {
		always ((no Node.edges + Node.fruit) iff (no (Node.edges + Node.fruit)))
	} is sat
	unionTighterThanNoTheorem : {
		always ((no Node.edges + Node.fruit) iff (no (Node.edges + Node.fruit)))
	} is theorem
	differenceTighterThanNoTheorem : {
		always ((no Node.edges - Node.fruit) iff (no (Node.edges - Node.fruit)))
	} is sat
	differenceTighterThanNoTheorem : {
		always ((no Node.edges - Node.fruit) iff (no (Node.edges - Node.fruit)))
	} is theorem
	unionTighterThanSomeTheorem : {
		always ((some Node.edges + Node.fruit) iff (some (Node.edges + Node.fruit)))
	} is sat
	unionTighterThanSomeTheorem : {
		always ((some Node.edges + Node.fruit) iff (some (Node.edges + Node.fruit)))
	} is theorem
	differenceTighterThanSomeTheorem : {
		always ((some Node.edges - Node.fruit) iff (some (Node.edges - Node.fruit)))
	} is sat
	differenceTighterThanSomeTheorem : {
		always ((some Node.edges - Node.fruit) iff (some (Node.edges - Node.fruit)))
	} is theorem
	unionTighterThanLoneTheorem : {
		always ((lone Node.edges + Node.fruit) iff (lone (Node.edges + Node.fruit)))
	} is sat
	unionTighterThanLoneTheorem : {
		always ((lone Node.edges + Node.fruit) iff (lone (Node.edges + Node.fruit)))
	} is theorem
	differenceTighterThanLoneTheorem : {
		always ((lone Node.edges - Node.fruit) iff (lone (Node.edges - Node.fruit)))
	} is sat
	differenceTighterThanLoneTheorem : {
		always ((lone Node.edges - Node.fruit) iff (lone (Node.edges - Node.fruit)))
	} is theorem
	unionTighterThanOneTheorem : {
		always ((one Node.edges + Node.fruit) iff (one (Node.edges + Node.fruit)))
	} is sat
	unionTighterThanOneTheorem : {
		always ((one Node.edges + Node.fruit) iff (one (Node.edges + Node.fruit)))
	} is theorem
	differenceTighterThanOneTheorem : {
		always ((one Node.edges - Node.fruit) iff (one (Node.edges - Node.fruit)))
	} is sat
	differenceTighterThanOneTheorem : {
		always ((one Node.edges - Node.fruit) iff (one (Node.edges - Node.fruit)))
	} is theorem
}

test expect {
	noTighterThanNegSat : {
		always ((! no Node) iff !(no Node))
	} is sat
	noTighterThanNegTheorem : {
		always ((! no Node) iff !(no Node))
	} is theorem
	noTighterThanNotSat : {
		always ((not no Node) iff not (no Node))
	} is sat
	noTighterThanNotTheorem : {
		always ((not no Node) iff not (no Node))
	} is theorem
	someTighterThanNegSat : {
		always ((! some Node) iff !(some Node))
	} is sat
	someTighterThanNegTheorem : {
		always ((! some Node) iff !(some Node))
	} is theorem
	someTighterThanNotSat : {
		always ((not some Node) iff not (some Node))
	} is sat
	someTighterThanNotTheorem : {
		always ((not some Node) iff not (some Node))
	} is theorem
	loneTighterThanNegSat : {
		always ((! lone Node) iff !(lone Node))
	} is sat
	loneTighterThanNegTheorem : {
		always ((! lone Node) iff !(lone Node))
	} is theorem
	loneTighterThanNotSat : {
		always ((not lone Node) iff not (lone Node))
	} is sat
	loneTighterThanNotTheorem : {
		always ((not lone Node) iff not (lone Node))
	} is theorem
	oneTighterThanNegSat : {
		always ((! one Node) iff !(one Node))
	} is sat
	oneTighterThanNegTheorem : {
		always ((! one Node) iff !(one Node))
	} is theorem
	oneTighterThanNotSat : {
		always ((not one Node) iff not (one Node))
	} is sat
	oneTighterThanNotTheorem : {
		always ((not one Node) iff not (one Node))
	} is theorem
}

test expect {
	negTighterThanInSat : {
		always ((Node.edges ! in fruit.Node) iff !(Node.edges in fruit.Node))
	} is sat
	negTighterThanInTheorem : {
		always ((Node.edges ! in fruit.Node) iff !(Node.edges in fruit.Node))
	} is theorem
	notTighterThanInSat : {
		always ((Node.edges not in fruit.Node) iff not (Node.edges in fruit.Node))
	} is sat
	notTighterThanInTheorem : {
		always ((Node.edges not in fruit.Node) iff not (Node.edges in fruit.Node))
	} is theorem
	negTighterThanEqSat : {
		always ((Node.edges != fruit.Node) iff !(Node.edges = fruit.Node))
	} is sat
	negTighterThanEqTheorem : {
		always ((Node.edges != fruit.Node) iff !(Node.edges = fruit.Node))
	} is theorem
	notTighterThanEqSat : {
		always ((Node.edges not = fruit.Node) iff not (Node.edges = fruit.Node))
	} is sat
	notTighterThanEqTheorem : {
		always ((Node.edges not = fruit.Node) iff not (Node.edges = fruit.Node))
	} is theorem
	negTighterThanIntEqSat : {
		always (all i1, i2 : Int | sum[i1] != sum[i2] iff !(sum[i1] = sum[i2]))
	} is sat
	negTighterThanIntEqTheorem : {
		always (all i1, i2 : Int | sum[i1] != sum[i2] iff !(sum[i1] = sum[i2]))
	} is theorem
	notTighterThanIntEqSat : {
		always (all i1, i2 : Int | sum[i1] not = sum[i2] iff not (sum[i1] = sum[i2]))
	} is sat
	notTighterThanIntEqTheorem : {
		always (all i1, i2 : Int | sum[i1] not = sum[i2] iff not (sum[i1] = sum[i2]))
	} is theorem
	negTighterThanLtSat : {
		always (all i1, i2 : Int | sum[i1] !< sum[i2] iff !(sum[i1] < sum[i2]))
	} is sat
	negTighterThanLtTheorem : {
		always (all i1, i2 : Int | sum[i1] !< sum[i2] iff !(sum[i1] < sum[i2]))
	} is theorem
	notTighterThanLtSat : {
		always (all i1, i2 : Int | sum[i1] not < sum[i2] iff not (sum[i1] < sum[i2]))
	} is sat
	notTighterThanLtTheorem : {
		always (all i1, i2 : Int | sum[i1] not < sum[i2] iff not (sum[i1] < sum[i2]))
	} is theorem
	negTighterThanGtSat : {
		always (all i1, i2 : Int | sum[i1] ! > sum[i2] iff !(sum[i1] > sum[i2]))
	} is sat
	negTighterThanGtTheorem : {
		always (all i1, i2 : Int | sum[i1] ! > sum[i2] iff !(sum[i1] > sum[i2]))
	} is theorem
	notTighterThanGtSat : {
		always (all i1, i2 : Int | sum[i1] not > sum[i2] iff not (sum[i1] > sum[i2]))
	} is sat
	notTighterThanGtTheorem : {
		always (all i1, i2 : Int | sum[i1] not > sum[i2] iff not (sum[i1] > sum[i2]))
	} is theorem
	negTighterThanGeqSat : {
		always (all i1, i2 : Int | sum[i1] ! >= sum[i2] iff !(sum[i1] >= sum[i2]))
	} is sat
	negTighterThanGeqTheorem : {
		always (all i1, i2 : Int | sum[i1] ! >= sum[i2] iff !(sum[i1] >= sum[i2]))
	} is theorem
	notTighterThanGeqSat : {
		always (all i1, i2 : Int | sum[i1] not >= sum[i2] iff not (sum[i1] >= sum[i2]))
	} is sat
	notTighterThanGeqTheorem : {
		always (all i1, i2 : Int | sum[i1] not >= sum[i2] iff not (sum[i1] >= sum[i2]))
	} is theorem
	negTighterThanLeqSat : {
		always (all i1, i2 : Int | sum[i1] ! <= sum[i2] iff !(sum[i1] <= sum[i2]))
	} is sat
	negTighterThanLeqTheorem : {
		always (all i1, i2 : Int | sum[i1] ! <= sum[i2] iff !(sum[i1] <= sum[i2]))
	} is theorem
	notTighterThanLeqSat : {
		always (all i1, i2 : Int | sum[i1] not <= sum[i2] iff not (sum[i1] <= sum[i2]))
	} is sat
	notTighterThanLeqTheorem : {
		always (all i1, i2 : Int | sum[i1] not <= sum[i2] iff not (sum[i1] <= sum[i2]))
	} is theorem
}
