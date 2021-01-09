#lang forge

sig Orange {}

sig Apple {
    org : set Orange
}

sig Peach {
    var per : set Pear,
    var mang : set Mango
}

sig Pear {}

sig Mango {
    var perz : set Pear
}

sig Grape {
    mel : set Melon
}

sig Raisin {
    var on : set Melon
}

var sig Melon {}

test expect nonVarCantChange {
    //non-var sig, no relations
    orangeCantChange : {not Orange' = Orange} is unsat
    orangeCanStaySame : {Orange' = Orange} is sat
    //non-var sig, non-var relation
    appleCantChange : {not Apple' = Apple} is unsat
    appleCanStaySame : {Apple' = Apple} is sat
    //non-var sig, has var relation
    peachCantChange : {not Peach' = Peach} is unsat
    peachCanStaySame : {Peach' = Peach} is sat
    //non-var sig,  referenced by var relation, has no relations
    pearCantChange : {not Pear' = Pear} is unsat
    pearCanStaySame : {Pear' = Pear} is sat
    //non-var sig, referenced by var relation, has var relation
    mangoCantChange : {not Mango' = Mango} is unsat
    mangoCanStaySame : {Mango' = Mango} is sat
    mangoNoChangeEvenIfPerzChange : {
        (not perz' = perz) and (Mango' = Mango)
    } is sat
    //non-var sig, has non-var relation to var sig
    grapeCantChange : {not Grape' = Grape} is unsat
    grapeCanStaySame : {Grape' = Grape} is sat
    //non-var sig, has var relation to var sig
    raisinCantChange : {not Raisin' = Raisin} is unsat
    raisinCanStaySame : {Raisin' = Raisin} is sat
    raisinNoChangeEvenIfOnChange : {
        (not on' = on) and (Raisin' = Raisin)
        or
        (not Melon' = Melon) and (Raisin' = Raisin)
    } is sat
}

var sig Cat {}

var sig Mouse {
    purr : set Cat
}