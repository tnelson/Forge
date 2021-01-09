#lang forge

sig Apple {
    var org : set Orange
}

sig Orange {}

pred prog {
    Orange' = Orange - Apple.org
}

test expect OrangeCantChange {
    //Since Orange is var, it can't change
    orangeDoesNotChange : {not always Orange' = Orange} is unsat
    orangeStaysSame : {always Orange' = Orange} is sat
    //If org is non-empty, then Orange' = Orange,
    //so org must always be empty
    orgMustBeEmpty : {not always no org} is unsat
    orgIsEmpty : {always no org} is sat
    //Since org is always empty, it never changes even though it is var
    
}