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

test expect {
  -- the `set` field's default breaker must respect piecewise `in` bounds
  setFieldKeepsPiecewise: { all n: N | n->n in r } for pwIn is unsat
  -- named atoms of an ancestor make the order non-WLOG (Y,X,Z is a valid order)
  linearNamedAncestor: { no g & next } for namedParent is sat
}
