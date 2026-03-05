#lang forge

option run_sterling off


option verbose 0
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

test expect nonVarRelsDontChange {
    nonVarToNonVarCantChange : {not always nonVarToNonVar' = nonVarToNonVar} is forge_error
    nonVarToNonVarCanStay : {always nonVarToNonVar' = nonVarToNonVar} is forge_error
    nonVarToVarCantChange : {not always nonVarToVar' = nonVarToVar} is forge_error
    nonVarToVarCanStay : {always nonVarToVar' = nonVarToVar} is forge_error
    varToNonVarCantChange : {not always varToNonVar' = varToNonVar} is forge_error
    varToNonVarCanStay : {always varToNonVar' = varToNonVar} is forge_error
    varToVarCantChange : {not always varToVar' = varToVar} is forge_error
    varToVarCanStay : {always varToVar' = varToVar} is forge_error
    nonVarBothToVarCantChange : {not always nonVarBothToVar' = nonVarBothToVar} is forge_error
    nonVarBothToVarCanStay : {always nonVarBothToVar' = nonVarBothToVar} is forge_error
    varBothToNonVarCantChange : {not always varBothToNonVar' = varBothToNonVar} is forge_error
    varBothToNonVarCanStay : {always varBothToNonVar' = varBothToNonVar} is forge_error
}


test expect VarRelsMaybeChange {
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
