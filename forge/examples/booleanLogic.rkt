#lang forge

------------------------------------------------------
-- Formula Type
------------------------------------------------------

abstract sig Formula {
  eval: set Instance -- Instances this is true in
}
sig Var extends Formula {} {
  eval = {i: Instance | this in i.trueVars}
}

sig Not extends Formula {child: one Formula}
sig And extends Formula {aleft, aright: one Formula}
sig Or extends Formula {oleft, oright: one Formula}

pred children {
  all n: Not | n.eval = Instance - n.child.eval
  all a: And | a.eval = a.aleft.eval & a.aright.eval
  all o: Or | o.eval = o.oleft.eval + o.oright.eval
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

--GiveMeAFormula : run {children and wellFormed and some Formula} for 5 Formula, 5 Instance

pred GiveMeABigFormula {
  children 
  wellFormed 
  some f: Formula | {
    #(allSubformulas[f] & Var) > 2
    some i: Instance | i not in f.eval
    some i: Instance | i in f.eval
  }
}

run GiveMeABigFormula for 8 Formula, 2 Instance -- need 2 instances

------------------------------------------------------
-- Semantics
------------------------------------------------------

sig Instance {
  trueVars: set Var
}



