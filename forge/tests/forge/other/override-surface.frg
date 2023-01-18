#lang forge

option run_sterling off


--Arity errors are checked by forge/tests/forge-core/other/override-core.rkt

abstract sig Parent {}

one sig P1 extends Parent {}
one sig P2 extends Parent {}
one sig P3 extends Parent {}

abstract sig Family {
	fp: set Parent,
	ffp: set Family->Parent,
	ffpfp: set Family->Parent->Family->Parent
}

one sig F1 extends Family {}
one sig F2 extends Family {}
one sig F3 extends Family {}

pred overArity2 {
	F1->P1 in (fp ++ F1->P1)
	F1->P2 not in fp++F1->P1
	not F1->P3 in fp ++ F1->P1
	one F1.(fp ++ F1->P1)
}

test expect {
	arity2: {overArity2} is theorem
}

pred overArity3 {
	F2->F2->P2 in (ffp ++ F2->F2->P2)
	one F2.(ffp ++ F2->F2->P2)
}

test expect {
	arity3: {overArity3} is theorem
}

-- Detect issues with arities larger than 3
pred overArity5 {
	F3->F1->P3->F2->P1 in ffpfp ++ F3->F1->P3->F2->P1
	one F3.(ffpfp++F3->F1->P3->F2->P1) 
}

test expect {
	arity5: {overArity5} is theorem
}

pred overEntireRelation {
	F2->fp in (ffp ++ F2->fp)
	(some fp) implies (fp = F2.(ffp ++ F2->fp))
}

test expect {
	overrideWithEntireRelation: {overEntireRelation} is theorem
}
