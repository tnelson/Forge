#lang forge
option run_sterling off
option verbose 0

/* Sibling sigs must be disjoint, like two top-level sigs must be. 
   A user-provided partial instance can't be allowed to force a non-empty intersection. */

sig P {}
one sig OneA, OneB extends P {}
sig C, D extends P {}
sig E extends C {}

test expect siblingDisjoint {
  one_one_overlap: { } for { P = `X  OneA = `X  OneB = `X } is forge_error "OneA and OneB"
  plain_overlap:   { } for { P = `X + `Y + `Z  OneA = `Y  OneB = `Z  C = `X  D = `X } is forge_error "C and D"
  nested_overlap:  { } for { P = `X + `Y + `Z  OneA = `Y  OneB = `Z  C in `X  E = `X  D = `X } is forge_error "C and D"

  // Overlapping _upper_ bounds are OK.
  upper_overlap_unsat: { some C & D } for { P = `X + `Y + `Z + `W  OneA = `Y  OneB = `Z  C in `X + `W  D in `X + `W } is unsat
  upper_overlap_sat:   { `X in C  some D } for { P = `X + `Y + `Z + `W  OneA = `Y  OneB = `Z  C in `X + `W  D in `X + `W } is sat
  distinct_ok:         { some C and some D } for { P = `X + `Y + `Z + `W  OneA = `Y  OneB = `Z  C = `X  D = `W } is sat
}
