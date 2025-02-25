#lang forge

option backend smtlibtor
option verbose 0

abstract sig Person {
	father: lone Man,
	mother: lone Woman
	}

sig Man extends Person {
	wife: lone Woman
	}

sig Woman extends Person {
	husband: lone Man
	}

pred model_facts {
	no p: Person | p in p.^(mother+father)
	wife = ~husband
	}


pred NoSelfFather {
	no m: Man | m = m.father
}
// This should not find any counterexample.
test expect {
    gp_nsf: {model_facts => NoSelfFather} is checked
}


//fun grandpas [p: Person] : set Person {
//	p.(mother+father).father
//	}
//
//pred ownGrandpa [p: Person] {
//	p in p.grandpas
//	}
//// This should not find any instance.
//run ownGrandpa for 4 Person
