#lang forge

var sig Alpha {}

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
    no org implies Orange in (Orange.org)'
    Orange in (Orange.org)' implies no org
}

//Since OrangeSwitch switches every time,
//no org and Orange in (Orange.org)' won't always be true,
//but they will always eventually be true
//technically in the last state the one which isn't
//true will never be eventually true again
//What is supposed to happen in that case?
test expect AlwaysEventuallyTrueButNotAlwaysTrue {
    noOrgNotAlwaysTrue : {always OrangeSwitch and always (no org)} is unsat
    allOrangeCoveredByOrgNotAlwaysTrue : {
        always OrangeSwitch
        always (Orange in (Orange.org)')
    } is unsat
    noOrgCanBeAlwaysEventuallyTrue : {
        always OrangeSwitch
        always eventually (no org)
    } is sat
    allOrangeCoveredByOrgCanBeAlwaysEventuallyTrue : {
        OrangeSwitch
        always eventually (Orange in (Orange.org)')
    } is sat
    noOrgMustBeAlwaysEventuallyTrue : {
        always OrangeSwitch
        not always eventually (no org)
    } is unsat
    allOrangeCoveredByOrgMustBeAlwaysEventuallyTrue : {
        OrangeSwitch
        not always eventually (Orange in (Orange.org)')
    } is unsat
}