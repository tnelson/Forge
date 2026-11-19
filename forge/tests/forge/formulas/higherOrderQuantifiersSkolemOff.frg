#lang forge

option run_sterling off
option verbose 0
option skolem_depth -1

/*
  If Skolem depth is _off_ (-1), no higher-order quantification is supported.
*/

sig A {}

test expect {
  someSet: { some X: set A | #X > 1 } for exactly 3 A is forge_error "disabled"
}
