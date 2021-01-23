#lang forge

option problem_type temporal
option verbose 0

sig Apple {
    var org : set Orange
}
var sig Orange {}

pred prog {
    Orange' = Orange - Apple.org
}

test expect OrangeChangesSoOrgMustChange {
    noChangeIfOrgEmpty : {prog and (no org) and (Orange' != Orange)} is unsat
    mustChangeIfOrgNotEmpty : {prog and (some org) and (Orange' = Orange)} is unsat
    //Since Orange' = Orange - Apple.org,
    //none of the oranges used by Apple.org are still available for Apple.org'
    //so a change in Orange forces a change in org
    orgCanBeEmptyInSecondState : {prog and (some org) and (org' = org - org)} is sat
    orgMustBeEmptyInSecondState : {prog and (some org) and (org' != org - org)} is unsat
}
