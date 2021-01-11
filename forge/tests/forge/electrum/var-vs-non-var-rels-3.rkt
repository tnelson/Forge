#lang forge

--Had To Split into part 1/2/3 due to it running out of memory in DrRacket :((

option problem_type temporal

sig nonVarSigNoRels {}
var sig VarSigNoRels {}
sig nonVarSigNonVarRels {
    nonVarToNonVar : set nonVarSigNoRels,
    nonVarToVar : set VarSigNoRels
}
var sig VarSigNonVarRels {
    varToNonVar : set nonVarSigNonVarRels,
    varToVar : set VarSigNoRels
}
sig nonVarSigVarRels {
    var varFromNonVarToNonVar : set nonVarSigNoRels,
    var varFromNonVarToVar : set VarSigNonVarRels
}
var sig VarSigVarRels {
    var varFromVarToNonVar : set nonVarSigVarRels,
    var varFromVarToVar : set VarSigVarRels
}
sig nonVarSigBothRels {
    var nonVarBothToNonVar : set nonVarSigVarRels,
    nonVarBothToVar : set VarSigBothRels
}
sig VarSigBothRels {
    varBothToNonVar : set nonVarSigBothRels,
    var varBothToVar : set VarSigNonVarRels
}

/*
Part 1
test expect nonVarRelsDontChange {
    nonVarToNonVarCantChange : {not always nonVarToNonVar' = nonVarToNonVar} is unsat
    nonVarToNonVarCanStay : {always nonVarToNonVar' = nonVarToNonVar} is sat
    nonVarToVarCantChange : {not always nonVarToVar' = nonVarToVar} is unsat
    nonVarToVarCanStay : {always nonVarToVar' = nonVarToVar} is sat
    varToNonVarCantChange : {not always varToNonVar' = varToNonVar} is unsat
    varToNonVarCanStay : {always varToNonVar' = varToNonVar} is sat
    varToVarCantChange : {not always varToVar' = varToVar} is unsat
    varToVarCanStay : {always varToVar' = varToVar} is sat
    nonVarBothToVarCantChange : {not always nonVarBothToVar' = nonVarBothToVar} is unsat
    nonVarBothToVarCanStay : {always nonVarBothToVar' = nonVarBothToVar} is sat
    varBothToNonVarCantChange : {not always varBothToNonVar' = varBothToNonVar} is unsat
    varBothToNonVarCanStay : {always varBothToNonVar' = varBothToNonVar} is sat
}
*/

test expect VarRelsMaybeChange {
    /*
    //Part 2
    varFromNonVarToNonVarCanAlwaysChange : {
        always varFromNonVarToNonVar' != varFromNonVarToNonVar
    } is sat
    varFromNonVarToNonVarCanSometimesChange : {
        (not always varFromNonVarToNonVar' != varFromNonVarToNonVar)
        and
        (not always varFromNonVarToNonVar' = varFromNonVarToNonVar)
    } is sat
    varFromNonVarToNonVarNoHaveToChange : {
        always varFromNonVarToNonVar' = varFromNonVarToNonVar
    } is sat
    varFromNonVarToVarCanAlwaysChange : {
        always varFromNonVarToVar' != varFromNonVarToVar
    } is sat
    varFromNonVarToVarCanSometimesChange : {
        (not always varFromNonVarToVar' != varFromNonVarToVar)
        and
        (not always varFromNonVarToVar' = varFromNonVarToVar)
    } is sat
    varFromNonVarToVarNoHaveToChange : {
        always varFromNonVarToVar' = varFromNonVarToVar
    } is sat
    varFromVarToNonVarCanAlwaysChange : {
        always varFromVarToNonVar' != varFromVarToNonVar
    } is sat
    varFromVarToNonVarCanSometimesChange : {
        (not always varFromVarToNonVar' != varFromVarToNonVar)
        and
        (not always varFromVarToNonVar' = varFromVarToNonVar)
    } is sat
    varFromVarToNonVarNoHaveToChange : {
        always varFromVarToNonVar' = varFromVarToNonVar
    } is sat
    */
    //Part 3
    varFromVarToVarCanAlwaysChange : {
        always varFromVarToVar' != varFromVarToVar
    } is sat
    varFromVarToVarCanSometimesChange : {
        (not always varFromVarToVar' != varFromVarToVar)
        and
        (not always varFromVarToVar' = varFromVarToVar)
    } is sat
    varFromVarToVarNoHaveToChange : {
        always varFromVarToVar' = varFromVarToVar
    } is sat
    nonVarBothToNonVarCanAlwaysChange : {
        always nonVarBothToNonVar' != nonVarBothToNonVar
    } is sat
    nonVarBothToNonVarCanSometimesChange : {
        (not always nonVarBothToNonVar' != nonVarBothToNonVar)
        and
        (not always nonVarBothToNonVar' = nonVarBothToNonVar)
    } is sat
    nonVarBothToNonVarNoHaveToChange : {
        always nonVarBothToNonVar' = nonVarBothToNonVar
    } is sat
    varBothToVarCanAlwaysChange : {
        always varBothToVar' != varBothToVar
    } is sat
    varBothToVarCanSometimesChange : {
        (not always varBothToVar' != varBothToVar)
        and
        (not always varBothToVar' = varBothToVar)
    } is sat
    varBothToVarNoHaveToChange : {
        always varBothToVar' = varBothToVar
    } is sat
}