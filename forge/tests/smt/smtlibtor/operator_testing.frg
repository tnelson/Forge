#lang forge

option backend smtlibtor

sig Name {}
sig Date {}
sig BirthdayBook {known: set Name, date: pfunc Name -> Date}

pred addDate[bb : BirthdayBook, bb1 : BirthdayBook, n : Name, d : Date] {
    bb1.date = bb.date ++ (n->d)
}

pred addDateTest {
    some bb, bb1: BirthdayBook | some n : Name | some d : Date | addDate[bb, bb1, n, d]
}

test expect { 
    override: {addDateTest} is sat
}

abstract sig Parent {}

one sig P1 extends Parent {}
one sig P2 extends Parent {}
one sig P3 extends Parent {}

abstract sig Family {
	fp1: set Parent,
	ffp: set Family->Parent,
	ffpfp: set Family->Parent->Family->Parent
}

one sig F1 extends Family {}
one sig F2 extends Family {}
one sig F3 extends Family {}

pred overArity2 {
	F1->P1 in (fp1 ++ F1->P1)
	F1->P2 not in fp1++F1->P1
	not F1->P3 in fp1 ++ F1->P1
	one F1.(fp1 ++ F1->P1)
}

test expect {
	arity2: {overArity2} is theorem
}
// Issues over arity 2?
// pred overArity3 {
// 	F2->F2->P2 in (ffp ++ F2->F2->P2)
// 	one F2.(ffp ++ F2->F2->P2)
// }
// test expect {
// 	arity3: {overArity3} is theorem
// }

// -- Detect issues with arities larger than 3
// pred overArity5 {
// 	F3->F1->P3->F2->P1 in ffpfp ++ F3->F1->P3->F2->P1
// 	one F3.(ffpfp++F3->F1->P3->F2->P1) 
// }

// test expect {
// 	arity5: {overArity5} is theorem
// }

// pred overEntireRelation {
// 	F2->fp1 in (ffp ++ F2->fp1)
// 	(some fp1) implies (fp1 = F2.(ffp ++ F2->fp1))
// }

// test expect {
// 	overrideWithEntireRelation: {overEntireRelation} is theorem
// }