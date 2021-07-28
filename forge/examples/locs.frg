#lang forge

option problem_type temporal

sig A {}
sig B {}
sig C {}
sig D {}
sig E {}

one sig oA {}
one sig aB {}
one sig oC {}

abstract sig F {}
abstract sig G {}

var sig X {}
var one sig Y {}
var abstract sig Z {}

/*
sig A {
	a: set A
}
sig B {
	b: set B
}
sig C {
	c: set C
}
sig D {
	d: set D
}
sig E {
	e: set e
}

one sig oA {
	oa: set oA
}
one sig aB {
	ab: set aB
}
one sig oC {
	oc: set oC
}

abstract sig F {
	f: set F
}
abstract sig G {
	g: set G
}

var sig X {
	x: set X
}
var one sig Y {
	y: set Y
}
var abstract sig Z {
	z: set Z
}
*/
