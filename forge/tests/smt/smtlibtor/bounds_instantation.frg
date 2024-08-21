#lang forge

option backend smtlibtor

abstract sig Position {}
one sig Near extends Position {}
one sig Far extends Position {}

abstract sig Animal {}
sig Cat extends Animal {}
sig Dog extends Animal {}

sig A {}
sig B extends A {}
sig C extends A {}
sig B1 extends B {}
sig B2 extends B {}

abstract sig Person {}
one sig King extends Person {}
sig Peasant extends Person {}

test expect {
    position_instantiation : {all p: Position | p in Near + Far} is sat
    animal_instantiation : {all a: Animal | a in Cat + Dog} is sat
    a_instantiation : {all a: A | a in B + C} is sat
    b_instantiation : {all b: B | b in B1 + B2} is sat
    person_instantiation : {all p: Person | p in King + Peasant} is sat
}