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

fun grandpas [p: Person] : set Person {
	p.(mother+father).father
}

pred ownGrandpa [p: Person] {
	p in p.grandpas
}

pred NoSelfGrandpa {
	no p: Person | p in grandpas[p]
}

test expect {
    gp_nsg: {model_facts => NoSelfGrandpa} is theorem
}
