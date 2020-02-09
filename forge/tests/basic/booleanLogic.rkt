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

expect gimme_big {
 GiveMeABigFormula for 8 Formula, 2 Instance is sat
 GiveMeABigFormula for 8 Formula, 1 Instance is unsat
}

expect {
 {some f: Formula | f in Var and f in And} is unsat
 {some x: univ | x in Formula and x in Instance} is unsat
 {some oleft & aleft} is unsat
 {semantics and {some n: Not, i: Instance | i in n.truth and i in n.child.truth }} for 8 Formula, 1 Instance is unsat
}

-- #f in bounds issue; not "test" yet
--expect instances {
--  {bind Var = V1 + V2 + V3 + V4 + V5 | #Var = 5 } is sat
--  {bind Var = V1 + V2 + V3 + V4 + V5 | #Var = 4 } is unsat
--}

pred localTautology[f: Formula] {
  -- true in all instances that Forge bothered to create
  all i: Instance | i in f.truth
}

pred generateInstances {
  -- force the existence of all instances needed
  all i: Instance | all v: Var |
      some i': Instance-i | v in i.trueVars => i'.trueVars = i.trueVars - v else i'.trueVars = i.trueVars + v 
}



test expect fancyBoundsExpectBlockName {
-- TODO: auto-detect increase in scope?

    initialFancyBounds: { some Formula }
    for 5 Formula
    for {    
        Var = Formula0 + Formula1 + Formula4
        Not = none
        And = Formula2
        Or =  Formula3        
        Formula = Formula0 + Formula1 + Formula2 + Formula3 + Formula4
        Instance = none
        child = none->none    
        oleft = Formula3->Formula0
        oright = Formula3->Formula0
        aleft = Formula2->Formula0
        aright = Formula2->Formula0
    } is sat

    initialContradictoryFancyBounds: { some Formula }
    for 5 Formula
    for {    
        Var = Formula0 + Formula1 + Formula4
        Not = Formula0 -- should contradict axioms
        And = Formula2
        Or =  Formula3        
        Formula = Formula0 + Formula1 + Formula2 + Formula3 + Formula4
        Instance = none
        child = none->none    
        oleft = Formula3->Formula0
        oright = Formula3->Formula0
        aleft = Formula2->Formula0
        aright = Formula2->Formula0
    } is unsat

    initialFancyBoundsWithWrongCount: { #Formula != 5 }
    for 5 Formula
    for {    
        Var = Formula0 + Formula1 + Formula4
        Not = none
        And = Formula2
        Or =  Formula3        
        Formula = Formula0 + Formula1 + Formula2 + Formula3 + Formula4
        Instance = none
        child = none->none    
        oleft = Formula3->Formula0
        oright = Formula3->Formula0
        aleft = Formula2->Formula0
        aright = Formula2->Formula0
    } is unsat

    fewerFormulasFancyBounds: { some Formula }
    for 4 Formula
    for {    
        Var = Formula0 + Formula1
        Not = none
        And = Formula2
        Or =  Formula3
        Formula = Formula0 + Formula1 + Formula2 + Formula3
        Instance = none
        child = none->none    
        oleft = Formula3->Formula0
        oright = Formula3->Formula0
        aleft = Formula2->Formula0
        aright = Formula2->Formula0
    } is sat

    useANotFancyBounds: { some Formula }
    for 4 Formula
    for {    
        Var = Formula0
        Not = Formula1
        And = Formula2
        Or =  Formula3
        Formula = Formula0 + Formula1 + Formula2 + Formula3
        Instance = none
        child = Formula1->Formula0
        oleft = Formula3->Formula0
        oright = Formula3->Formula0
        aleft = Formula2->Formula0
        aright = Formula2->Formula0
    } is sat

    semanticsFancyBounds: { semantics wellFormed no truth}
    for 4 Formula
    for {    
        Var = Formula0
        Not = Formula1
        And = Formula2
        Or =  Formula3
        Formula = Formula0 + Formula1 + Formula2 + Formula3
        Instance = Instance0
        child = Formula1->Formula0
        oleft = Formula3->Formula0
        oright = Formula3->Formula0
        aleft = Formula2->Formula0
        aright = Formula2->Formula0
    } is unsat

    localTautologyFancyBounds: { semantics wellFormed some f: Formula | localTautology[f]}
    for 4 Formula
    for {    
        Var = Formula0
        Not = Formula1
        And = Formula2
        Or =  Formula3
        Formula = Formula0 + Formula1 + Formula2 + Formula3
        Instance = Instance0
        child = Formula1->Formula0
        oleft = Formula3->Formula0
        oright = Formula3->Formula0
        aleft = Formula2->Formula0
        aright = Formula2->Formula0
    } is sat

    generateNotEnough: { semantics wellFormed generateInstances }
    for 4 Formula
    for {    
        Var = Formula0
        Not = Formula1
        And = Formula2
        Or =  Formula3
        Formula = Formula0 + Formula1 + Formula2 + Formula3
        Instance = Instance0
        child = Formula1->Formula0
        oleft = Formula3->Formula0
        oright = Formula3->Formula0
        aleft = Formula2->Formula0
        aright = Formula2->Formula0
    } is unsat

    generateEnough: { semantics wellFormed generateInstances }
    for 4 Formula
    for {    
        Var = Formula0
        Not = Formula1
        And = Formula2
        Or =  Formula3
        Formula = Formula0 + Formula1 + Formula2 + Formula3
        Instance = Instance0 + Instance1
        child = Formula1->Formula0
        oleft = Formula3->Formula0
        oright = Formula3->Formula0
        aleft = Formula2->Formula0
        aright = Formula2->Formula0
    } is sat

    generateEnoughAndSomeTautology: { semantics wellFormed generateInstances some f: Formula | localTautology[f] }
    for 5 Formula -- note 5 formula! also different instance than above (change p /\ p to p /\ q)
    for {    
        Var = Formula0 + Formula4
        Not = Formula1
        And = Formula2
        Or =  Formula3
        Formula = Formula0 + Formula1 + Formula2 + Formula3 + Formula4
        Instance = Instance0 + Instance1
        child = Formula1->Formula0
        oleft = Formula3->Formula0
        oright = Formula3->Formula4
        aleft = Formula2->Formula0
        aright = Formula2->Formula4
    } is unsat

} 
