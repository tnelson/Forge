#lang forge/froglet
option run_sterling off
option verbose 0

sig A {}

test expect {
  someSet: { some X: set A | some X } for exactly 3 A is forge_error "not used in Froglet"
}
