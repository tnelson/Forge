#lang forge

//Checks that non-var sigs can't change over time
//Checks that non-var relations can't change over time

sig Apple {
    org : set Orange
}

sig Orange {}

pred prog {
    Orange' = Orange - Apple.org
}

test expect orangeCantChange {
    --If org is non empty, then Orange' will be different from Orange
    orgMustBeEmpty : {prog and some org} is unsat
    --If org is empty, then Orange doesn't have to change so it's okay
    orgCanBeEmpty : {prog and no org} is sat
}

test expect orgCantChange {
    --can't change - it's not a var relation
    orgChangingIsUnsat : {not org' = org} is unsat
    --is okay if it stays the same
    orgNotChangingIsSay : {org' = org} is sat
}