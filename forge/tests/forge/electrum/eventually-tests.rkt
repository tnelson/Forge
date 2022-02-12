#lang forge

option problem_type temporal
option verbose 0

sig Stepper {
    var step : set Stepper //will be used to restrict posssible transitions
}

//Three types of transitions between states
//First prog1, then prog2, then prog3
//After the first prog2, prog1 is no longer allowed
//After the first prog3, prog2 is no longer allowed
//Note that it is possible to get to prog3 before prog2

//step must be empty here,
//but in prog2 and prog3 it will stop being empty,
//which means that next_state prog2 or prog3 this pred is no longer possible
pred prog1 {
    some Stepper
    no step
}

//step is no longer empty,
//but Stepper.step is not all of Stepper either
//step in step', so step' has at least as many elements as step,
//so step' cannot be empty, meaning prog1 can't be possible ever again
pred prog2 {
    some Stepper
    Stepper.step != Stepper
    some step
    step in step'
    next_state always step in step'
}

//Now, Stepper.step = Stepper,
//and step' = step, so it will always be true from now on that Stepper.step = Stepper,
//meaning that prog1 and prog2 will never be possible again
pred prog3 {
    some Stepper
    Stepper in Stepper.step
    step' = step
    next_state always step' = step
}

test expect EventuallyGeneral {
    noProg2AfterProg3 : {prog3 and eventually prog2} is unsat
    noProg1AfterProg3 : {prog3 and eventually prog1} is unsat
    noProg1AfterProg2 : {prog2 and eventually prog1} is unsat
    canHaveProg2andProg3AfterProg1 : {
        prog1
        eventually prog2
        eventually prog3
    } is sat
    canHaveProg1ThenProg2ButNeverProg3 : {
        prog1
        eventually prog2
        not eventually prog3
    } is sat
    canHaveProg1ThenProg3ButNeverProg2 : {
        prog1
        not eventually prog2
        eventually prog3
    } is sat
    canHaveProg3AfterProg2 : {
        prog2 and eventually prog3
    } is sat
}

var sig Alpha {}

pred clearAlpha {
    some Alpha
    Alpha' = Alpha - Alpha //should work instead of (no Alpha') for now
}

pred alphaStaysCleared {
    //once Alpha becomes empty,
    //it will be empty forever
    no Alpha implies Alpha' = Alpha - Alpha
}
/*
run {
    not (next_state eventually no Alpha)
    eventually no Alpha
    no Alpha
}
*/
test expect EventuallyIncludesCurrentState {
    clearAlphaCanBeTrue : {clearAlpha and alphaStaysCleared} is sat
    //After clearAlpha, Alpha will be empty forever
    //because of alphaStaysCleared
    //But clearAlpha contains some Alpha,
    //so Alpha cannot be empty when clearAlpha is true
    //meaning that next_state clearAlpha is true once,
    //it will never be true again
    //However, eventually clearAlpha is STILL true because
    //eventually is inclusive of the current state
    clearAlphaCanBeEventuallyTrue : {
        always alphaStaysCleared
        clearAlpha
        eventually clearAlpha
    } is sat
    clearAlphaMustBeEventuallyTrue : {
        always alphaStaysCleared
        clearAlpha
        not eventually clearAlpha
    } is unsat
}

test expect IfAfterTrueThenEventuallyTrue {
    noAlphaCanbeTrueAfterClearAlpha : {clearAlpha and next_state no Alpha} is sat
    noAlphaMustBeTrueAfterClearAlpha : {clearAlpha and next_state (not no Alpha)} is unsat
    noAlphaCanBeEventuallyTrueAfterClearAlpha : {
        clearAlpha
        eventually no Alpha
    } is sat
    noAlphaMustBeTrueAfterEventuallyTrue : {
        clearAlpha
        not (eventually no Alpha)
    } is unsat
    eventuallyClearAlphaMeansEventuallyAfterNoAlpha : {
        eventually clearAlpha
        not (eventually next_state no Alpha)
    } is unsat
}

test expect afterEventually {
    canHaveEventuallyNoAlpha : {eventually no Alpha} is sat
    canHaveAfterEventuallyNoAlpha : {next_state eventually no Alpha} is sat
    afterEventuallyNoAlphaMeansEventuallyNoAlpha : {
        next_state eventually no Alpha
        not (eventually no Alpha)
    } is unsat
    canHaveNotEventuallyNoAlpha : {not eventually no Alpha} is sat
    canHaveNotAfterEventuallyNoAlpha : {not (next_state eventually no Alpha)} is sat
    notAfterEventuallyNoAlphaCanHaveEventuallyNoAlpha : {
        not (next_state eventually no Alpha)
        eventually no Alpha
    } is sat
    someAlphaAndNotAfterEventuallyNoAlphaMeansNotEventuallyNoAlpha : {
        not (next_state eventually no Alpha)
        eventually no Alpha
        some Alpha
    } is unsat
}

pred alwaysTrue {
    always Alpha = Alpha
}

test expect AlwaysTrueIsAlwaysEventuallyTrue {
    alwaysTrueCanBeEventuallyTrue : {eventually alwaysTrue} is sat
    alwaysTrueCanBeAlwaysEventuallyTrue : {always eventually alwaysTrue} is sat
    alwaysTrueMustBeEventuallyTrue : {not eventually alwaysTrue} is unsat
    alwaysTrueMustBeAlwaysEventuallyTrue : {not (always eventually alwaysTrue)} is unsat
}

sig Orange {
    var org : set Orange
}

test expect EventuallyTrueAndEventuallyNotTrue {
    orgCanCoverAllOfOrange : {eventually Orange in Orange.org} is sat
    orgCanBeEmpty : {eventually no org} is sat
    orgCanDoBoth : {
        eventually Orange in Orange.org
        eventually no org
    } is sat
}

//note: this pred is not used in the above test expect
pred OrangeSwitch {
    always (no org or Orange in Orange.org)
    no org implies Orange in (Orange.org)'
    Orange in (Orange.org) implies org' = org - org
}

//Since OrangeSwitch switches every time,
//no org and Orange in (Orange.org)' won't always be true,
//but they will always eventually be true
//technically in the last state the one which isn't
//true will never be eventually true again
//What is supposed to happen in that case?
test expect AlwaysEventuallyTrueButNotAlwaysTrue {
    noOrgCanBeAlwaysEventuallyTrue : {
        always OrangeSwitch
        always eventually (no org)
    } is sat
    allOrangeCoveredByOrgCanBeAlwaysEventuallyTrue : {
        always OrangeSwitch
        always eventually (Orange in (Orange.org)')
    } is sat
    noOrgMustBeAlwaysEventuallyTrue : {
        always OrangeSwitch
        not always eventually (no org)
    } is unsat
    allOrangeCoveredByOrgMustBeAlwaysEventuallyTrue : {
        always OrangeSwitch
        not always eventually (Orange in (Orange.org)')
    } is unsat
}
