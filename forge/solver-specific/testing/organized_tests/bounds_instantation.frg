#lang forge

option backend smtlibtor

abstract sig Position {}
one sig Near extends Position {}
one sig Far extends Position {}

abstract sig Animal {}
sig Cat extends Animal {}
sig Dog extends Animal {}

test expect {
    position_instantiation : {all p: Position | p in Near + Far} is sat
    animal_instantiation : {all a: Animal | a in Cat + Dog} is sat
}