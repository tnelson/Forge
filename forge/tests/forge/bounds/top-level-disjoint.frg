#lang forge
option run_sterling off
option verbose 0

/* Top-level sigs must be disjoint. Confirm that doing this results in a Forge error. */

sig A {}
sig B {}
sig C extends B {}

test expect topLevelDisjoint {
  eq_overlap: { } for { A = `X  B = `X } is forge_error "Top-level sigs must be disjoint"
  in_overlap: { } for { A in `X + `Y  B in `X + `Z } is forge_error "share atom `X"
  eq_in_overlap: { } for { A = `X + `Y  B in `X + `Z } is forge_error "share atom `X"
  child_overlap: { } for { A = `X  B in `X + `Y  C = `X } is forge_error "share atom `X"
  distinct_ok: { some A and some B } for { A in `X + `Y  B in `Z } is sat
}
