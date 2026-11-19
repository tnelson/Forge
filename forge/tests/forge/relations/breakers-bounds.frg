#lang forge
option run_sterling off
option verbose 0

/* 
  Consistency between breakers and partial-instance bounds.
*/ 

sig N { r: set N }
inst pwIn { N = `N0 + `N1  `N0.r in `N1 }

abstract sig P { g: set P }
sig A extends P { next: lone A }
inst namedParent { P = `X + `Y + `Z  g = `X -> `Y  next is linear }

sig Desk { d: set N -> N }

test expect {
  -- the `set` field's default breaker must respect piecewise `in` bounds
  setFieldKeepsPiecewise: { all n: N | n->n in r } for pwIn is unsat
  -- named atoms of an ancestor make the order non-WLOG (Y,X,Z is a valid order)
  linearNamedAncestor: { no g & next } for namedParent is sat
  -- A linear order indexed by atom must not forced an index-atom to exist.
  linearOwnerMayBeEmpty: { no Desk } for 3 N, 2 Desk for { d is linear } is sat
  plinearOwnerMayBeEmpty: { no Desk } for 3 N, 2 Desk for { d is plinear } is sat
}
