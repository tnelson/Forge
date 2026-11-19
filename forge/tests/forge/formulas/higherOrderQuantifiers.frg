#lang forge

option run_sterling off
option verbose 0

/*
  Testing higher-order quantification via Skolemization 
*/

sig A {}

test expect {
  -- Skolemizable: effectively existential, under positive `and` only
  someSetSat:      { some X: set A | #X > 1 } for exactly 3 A is sat
  someSetUnsat:    { some X: set A | #X > 3 } for exactly 3 A is unsat
  notAllSetUnsat:  { not (all X: set A | #X < 4) } for exactly 3 A is unsat
  notAllSetSat:    { not (all X: set A | #X < 3) } for exactly 3 A is sat
  allSetChecked:   { all X: set A | X in A } for exactly 3 A is checked
  underAndSat:     { some A and (some X: set A | #X = 2) } for exactly 3 A is sat
  disjSetSat:      { some disj X, Y: set A | #X = 1 and #Y = 2 } for exactly 3 A is sat

  -- Not effectively existential
  allSetRun:       { all X: set A | #X < 4 } for exactly 3 A is forge_error "effectively existential"
  allSetUnsat:     { all X: set A | #X > 4 } for exactly 3 A is forge_error "effectively existential"
  notSomeSet:      { not (some X: set A | #X > 1) } for exactly 3 A is forge_error "effectively existential"
  noSet:           { no X: set A | #X = 3 } for exactly 3 A is forge_error "effectively existential"

  -- Contexts Pardinus does not skolemize under
  underOr:         { (some X: set A | #X > 1) or no A } for exactly 3 A is forge_error "under `or`"
  underIff:        { (some X: set A | #X > 1) iff no A } for exactly 3 A is forge_error "under `=>`"
  underAllDepth0:  { all a: A | some X: set A | a in X and #X = 2 } for exactly 3 A is forge_error "skolem_depth"
  inExpression:    { #{a: A | some X: set A | a in X} = 3 } for exactly 3 A is forge_error "inside an expression"

  -- `one` and `lone` quantifiers desugar to comprehensions
  oneSet:          { one X: set A | #X = 3 } for exactly 3 A is forge_error "only supported for `some`, `all`, and `no`"
  loneSet:         { lone X: set A | #X = 3 } for exactly 3 A is forge_error "only supported for `some`, `all`, and `no`"
}
