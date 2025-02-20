#lang forge 

option verbose 0
option run_sterling off 

sig A extends Int {}
one sig B extends Int {}

 // If this fails, the issue may influence later tests.
someB: assert {some B} is sat
emptyB: assert {no B} is unsat 

emptyA: assert {no A} is sat 
noOverlap: assert {some A & B} is unsat
properSubsetA: assert {some (Int - B)} is sat
properSubsetB: assert {some (Int - A)} is sat
Bis5: assert {B = 5} is sat
Bis0: assert {B = 0} is sat
BisMultiple: assert {#B = 2} is unsat

// Check how this works in the presence of a partial instance. 
Bis5_bnd5: assert {B = 5} is sat for {B = 5}
Bis6_bnd5: assert {B = 6} is unsat for {B = 5}