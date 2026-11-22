#lang forge

option run_sterling off
option verbose 0
option skolem_depth 1


/*
  Test Skolem Depth: all | some | ... needs depth >= 1
*/

sig A {}

test expect {
  underOneAll:  { all a: A | some X: set A | a in X and #X = 2 } for exactly 3 A is sat
  underTwoAlls: { all a, b: A | some X: set A | a in X and b in X } for exactly 3 A is forge_error "skolem_depth"
  underOneAllRel: { all a: A | some r: set A -> A | a->a in r and #r = 1 } for exactly 3 A is sat
}
