#lang forge
option run_sterling off
option verbose 0

/*
   Resolving bounds vs. scopes. Bounds cannot force the existence of more atoms 
   than the scope allows. However, if there is no declared scope, the default 
   will be raised to fit the given bounds.
*/

one sig A {}
sig N {}
sig P {}
one sig P1, P2, P3, P4, P5 extends P {}

test expect scopeVsBounds {
  one_eq_two:     { } for { A = `X + `Y } is forge_error "at least 2 atoms, but its scope allows at most 1"
  one_ni_two:     { } for { A ni `X + `Y } is forge_error "at least 2 atoms, but its scope allows at most 1"
  for1_eq_two:    { } for 1 N for { N = `X + `Y } is forge_error "Sig N must contain at least 2 atoms"
  for3_one_kids:  { } for 3 P is forge_error "Sig P must contain at least 5 atoms"

  no_scope_raised: { #P = 5 } is sat
  for5_one_kids:   { #P = 5 } for 5 P is sat
  for1_eq_one:     { #N = 1 } for 1 N for { N = `X } is sat
}
