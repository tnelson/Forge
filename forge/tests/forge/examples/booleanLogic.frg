#lang forge

option run_sterling off


option verbose 0

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
 big1: GiveMeABigFormula for 8 Formula, 2 Instance, 5 Int is sat
 big2: GiveMeABigFormula for 8 Formula, 1 Instance, 5 Int is unsat
}

test expect {
 {some f: Formula | f in Var and f in And} is unsat
 {some x: univ | x in Formula and x in Instance} is unsat
 {some oleft & aleft} is unsat
 {semantics and {some n: Not, i: Instance | i in n.truth and i in n.child.truth }} for 8 Formula, 1 Instance is unsat
}

pred localTautology[f: Formula] {
  -- true in all instances that Forge bothered to create
  all i: Instance | i in f.truth
}

pred generateInstances {
  -- force the existence of all instances needed
  all i: Instance | all v: Var |
      some i2: Instance-i | v in i.trueVars => i2.trueVars = i.trueVars - v else i2.trueVars = i.trueVars + v 
}



test expect fancyBoundsExpectBlockName {
-- TODO: auto-detect increase in scope?

    initialFancyBounds: { some Formula }
    for 5 Formula
    for {    
        Var = `Var0 + `Var1 + `Var2
        no Not
        And = `And0
        Or =  `Or0        
        Formula = `Var0 + `Var1 + `And0 + `Or0 + `Var2
        no Instance
        no child
        oleft = `Or0->`Var0
        oright = `Or0->`Var0
        aleft = `And0->`Var0
        aright = `And0->`Var0
    } is sat

    initialContradictoryFancyBounds: { some Formula }
    for 5 Formula
    for {    
        Var = `Var0 + `Var1 + `Var2
        Not = `Var0 -- should contradict axioms
        And = `And0
        Or =  `Or0        
        Formula = `Var0 + `Var1 + `And0 + `Or0 + `Var2
        no Instance
        no child
        oleft = `Or0->`Var0
        oright = `Or0->`Var0
        aleft = `And0->`Var0
        aright = `And0->`Var0
    } is unsat

    initialFancyBoundsWithWrongCount: { #Formula != 5 }
    for 5 Formula
    for {    
        Var = `Var0 + `Var1 + `Var2
        no Not
        And = `And0
        Or =  `Or0        
        Formula = `Var0 + `Var1 + `And0 + `Or0 + `Var2
        no Instance
        no child
        oleft = `Or0->`Var0
        oright = `Or0->`Var0
        aleft = `And0->`Var0
        aright = `And0->`Var0
    } is unsat

    fewerFormulasFancyBounds: { some Formula }
    for 4 Formula
    for {    
        Var = `Var0 + `Var1
        no Not
        And = `And0
        Or =  `Or0
        Formula = `Var0 + `Var1 + `And0 + `Or0
        no Instance
        no child
        oleft = `Or0->`Var0
        oright = `Or0->`Var0
        aleft = `And0->`Var0
        aright = `And0->`Var0
    } is sat

    useANotFancyBounds: { some Formula }
    for 4 Formula
    for {    
        Var = `Var0
        Not = `Not0
        And = `And0
        Or = `Or0
        Formula = `Var0 + `Not0 + `And0 + `Or0
        no Instance
        child = `Not0->`Var0
        oleft = `Or0->`Var0
        oright = `Or0->`Var0
        aleft = `And0->`Var0
        aright = `And0->`Var0
    } is sat

    semanticsFancyBounds: { semantics wellFormed no truth}
    for 4 Formula
    for {    
        Var = `Var0
        Not = `Not0
        And = `And0
        Or =  `Or0
        Formula = `Var0 + `Not0 + `And0 + `Or0
        Instance = `Instance0
        child = `Not0->`Var0
        oleft = `Or0->`Var0
        oright = `Or0->`Var0
        aleft = `And0->`Var0
        aright = `And0->`Var0
    } is unsat

    localTautologyFancyBounds: { semantics wellFormed some f: Formula | localTautology[f]}
    for 4 Formula
    for {    
        Var = `Var0
        Not = `Not0
        And = `And0
        Or =  `Or0
        Formula = `Var0 + `Not0 + `And0 + `Or0
        Instance = `Instance0
        child = `Not0->`Var0
        oleft = `Or0->`Var0
        oright = `Or0->`Var0
        aleft = `And0->`Var0
        aright = `And0->`Var0
    } is sat

    generateNotEnough: { semantics wellFormed generateInstances }
    for 4 Formula
    for {    
        Var = `Var0
        Not = `Not0
        And = `And0
        Or =  `Or0
        Formula = `Var0 + `Not0 + `And0 + `Or0
        Instance = `Instance0
        child = `Not0->`Var0
        oleft = `Or0->`Var0
        oright = `Or0->`Var0
        aleft = `And0->`Var0
        aright = `And0->`Var0
    } is unsat

    generateEnough: { semantics wellFormed generateInstances }
    for 4 Formula
    for {    
        Var = `Var0
        Not = `Not0
        And = `And0
        Or =  `Or0
        Formula = `Var0 + `Not0 + `And0 + `Or0
        Instance = `Instance0 + `Instance1
        child = `Not0->`Var0
        oleft = `Or0->`Var0
        oright = `Or0->`Var0
        aleft = `And0->`Var0
        aright = `And0->`Var0
    } is sat

    generateEnoughAndSomeTautology: { semantics wellFormed generateInstances some f: Formula | localTautology[f] }
    for 5 Formula -- note 5 formula! also different instance than above (change p /\ p to p /\ q)
    for {    
        Var = `Var0 + `Var1
        Not = `Not0
        And = `And0
        Or =  `Or0
        Formula = `Var0 + `Not0 + `And0 + `Or0 + `Var1
        Instance = `Instance0 + `Instance1
        child = `Not0->`Var0
        oleft = `Or0->`Var0
        oright = `Or0->`Var1
        aleft = `And0->`Var0
        aright = `And0->`Var1
    } is unsat

} 
