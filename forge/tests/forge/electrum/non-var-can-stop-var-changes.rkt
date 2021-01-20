#lang forge

option problem_type temporal
option verbose 0

sig Apple {
    var org : set Orange
}

sig Orange {}

pred prog {
    Orange' = Orange - Apple.org
}

test expect nonVarSigStopsVarRelChanges {
    //Since Orange is not var, it can't change
    orangeCantChange : {not always Orange' = Orange} is unsat
    orangeStaysSame : {always Orange' = Orange} is sat
    //If org is non-empty, then Orange' = Orange,
    //so org must always be empty
    orgMustBeEmpty : {prog and (not always no org)} is unsat
    orgIsEmpty : {prog and (always no org)} is sat
    //Since org is always empty, it never changes even though it is var
    orgCantChange : {prog and (not always org' = org)} is unsat
    orgStaysSame : {prog and (always org' = org)} is sat
}

sig Banana {
    parent : set BananaTree
}

//maybe some more trees grow :shrug: so this can change
var sig BananaTree {}

pred validTree {
    //Each banana grows on one BananaTree (though one BananaTree can grow several bananas)
    all b : Banana | one b.parent
    //Every BananaTree is always growing Bananas (for sake of the test cases)
    always BananaTree in Banana.parent
}

test expect nonVarRelStopsVarSigChange {
    //Since parent is not var, it can't change
    parentCantChange : {not always parent' = parent} is unsat
    parentStaysSame : {always parent' = parent} is sat
    //Since Banana is not var, it can't change
    bananaCantChange : {not always Banana' = Banana} is unsat
    bananaStaysSame : {always Banana' = Banana} is sat
    //Since Banana and parent can't change, neeither does Banana.parent
    parentTreeSetCantChange : {not always (Banana.parent)' = Banana.parent} is unsat
    parentTreeSetStaysSame : {always (Banana.parent)' = Banana.parent} is sat
    //Since every BananaTree is in Banana.parent, which stays the same
    //you can't gain BananaTrees (or Banana.parent would have to grow)
    //and you can't lose BananaTrees (or Banana.parent would lose members)
    //So, BananaTree can't change even though it is var
    bananaTreeCantChange : {
        validTree
        not always BananaTree' = BananaTree
    } is unsat
    bananaTreeStaysSame : {
        validTree
        always BananaTree' = BananaTree
    } is sat
}
