#lang forge

-- Taken from Tim's lecture code

------------------------------------------------------
-- Formula Type
------------------------------------------------------

abstract sig Formula {
  truth: set Instance -- Instances this is true in
}
sig Var extends Formula {}  
sig Not extends Formula {child: one Formula}
sig And extends Formula {aleft, aright: one Formula}
sig Or extends Formula {oleft, oright: one Formula}

sig Instance {
  trueVars: set Var
}

pred semantics {
  all n: Not | n.truth = Instance - n.child.truth
  all a: And | a.truth = a.aleft.truth & a.aright.truth
  all o: Or  | o.truth = o.oleft.truth + o.oright.truth
  all v: Var | v.truth = {i: Instance | v in i.trueVars}
}
-- IMPORTANT: don't add new formulas without updating allSubformulas and children

------------------------------------------------------
-- Axioms and helpers
------------------------------------------------------

fun allSubformulas[f: Formula]: set Formula {
  f.^(child + oleft + oright + aleft + aright)
}

pred wellFormed {
  -- no cycles
  all f: Formula | f not in allSubformulas[f]

  -- TODO: abstract
  all f: Formula | f in Not + And + Or + Var
}

------------------------------------------------------

pred GiveMeABigFormula {
  semantics
  wellFormed 
  some f: Formula | {
    #(allSubformulas[f] & Var) > 2
    some i: Instance | i not in f.truth
    some i: Instance | i in f.truth
  }
}

test expect gimme_big {
 GiveMeABigFormula for 8 Formula, 2 Instance is sat
 GiveMeABigFormula for 8 Formula, 1 Instance is unsat
}

test expect {
 {some f: Formula | f in Var and f in And} is unsat
 {some x: univ | x in Formula and x in Instance} is unsat
 {some oleft & aleft} is unsat
 {semantics and {some n: Not, i: Instance | i in n.truth and i in n.child.truth }} for 8 Formula, 1 Instance is unsat
}

-- #f in bounds issue; not "test" yet
expect instances {
  {bind Var = V1 + V2 + V3 + V4 + V5 | #Var = 5 } is sat
  {bind Var = V1 + V2 + V3 + V4 + V5 | #Var = 4 } is unsat
}
