#lang forge

//This takes a few minutes to run (on my machine at least)

option verbose 0
option problem_type temporal

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

var sig Melon {
    tom : set Tomato,
    var apr : set Apricot
}

sig Tomato {}

sig Apricot {}

test expect nonVarSigsCantChange {
    //non-var sig, no relations
    orangeCantChange : {not always Orange' = Orange} is unsat
    orangeCanStaySame : {always Orange' = Orange} is sat
    //non-var sig, non-var relation
    appleCantChange : {not always Apple' = Apple} is unsat
    appleCanStaySame : {always Apple' = Apple} is sat
    //non-var sig, has var relation
    peachCantChange : {not always Peach' = Peach} is unsat
    peachCanStaySame : {always Peach' = Peach} is sat
    //non-var sig,  referenced by var relation, has no relations
    pearCantChange : {not always Pear' = Pear} is unsat
    pearCanStaySame : {always Pear' = Pear} is sat
    //non-var sig, referenced by var relation, has var relation
    mangoCantChange : {not always Mango' = Mango} is unsat
    mangoCanStaySame : {always Mango' = Mango} is sat
    mangoNoChangeEvenIfPerzChange : {
        (not always perz' = perz) and (always Mango' = Mango)
    } is sat
    //non-var sig, has non-var relation to var sig
    grapeCantChange : {not always Grape' = Grape} is unsat
    grapeCanStaySame : {always Grape' = Grape} is sat
    //non-var sig, has var relation to var sig
    raisinCantChange : {not always Raisin' = Raisin} is unsat
    raisinCanStaySame : {always Raisin' = Raisin} is sat
    raisinNoChangeEvenIfOnOrMelonChange : {
        (not always on' = on) and (always Raisin' = Raisin)
        or
        (not always Melon' = Melon) and (always Raisin' = Raisin)
    } is sat
    //non-var sig, referenced by non-var relation from var sig
    tomatoCantChange : {not always Tomato' = Tomato} is unsat
    tomatoCanStaySame : {always Tomato' = Tomato} is sat
    //non-var sig, referenced by var relation from var sig
    apricotCantChange : {not always Apricot' = Apricot} is unsat
    apricotCanStaySame : {always Apricot' = Apricot} is sat
}

var sig Cat {}

var sig Mouse {
    var purr : set Cat
}

var sig Dove {
    snek : set Snake,
    hors : set Horse
}

var sig Snake {}

var sig Horse {
    sneks : set Snake
}

var sig Dog {
    var rabb : set Rabbit
}

var sig Frog {
    bit : set Rabbit
}

sig Rabbit {
    var chin : set Chinchilla,
    rhi : set Rhino
}

var sig Chinchilla {}

var sig Rhino {}

test expect varSigsCanChange {
    //var sig, no relations
    catCanAlwaysChange : {always Cat' != Cat} is sat
    catCanSometimesChange : {
        (not always Cat' = Cat) and (not always Cat' != Cat)
    } is sat
    catNoHaveToChange : {always Cat' = Cat} is sat
    //var sig, has var relation
    mouseCanAlwaysChange : {always Mouse' != Mouse} is sat
    mouseCanSometimesChange : {
        (not always Mouse' = Mouse) and (not always Mouse' != Mouse)
    } is sat
    mouseNoHaveToChange : {always Mouse' = Mouse} is sat
    //var sig, has non-var relations
    doveCanAlwaysChange : {always Dove' != Dove} is sat
    doveCanSometimesChange : {
        (not always Dove' = Dove) and (not always Dove' != Dove)
    } is sat
    doveNoHaveToChange : {always Dove' = Dove} is sat
    //var sig, referenced by non-var relation
    snakeCanAlwaysChange : {always Snake' != Snake} is sat
    snakeCanSometimesChange : {
        (not always Snake' = Snake) and (not always Snake' != Snake)
    } is sat
    snakeNoHaveToChange : {always Snake' = Snake} is sat
    //var sig, has non-var relation, referenced by non-var relation
    horseCanAlwaysChange : {always Horse' != Horse} is sat
    horseCanSometimesChange : {
        (not always Horse' = Horse) and (not always Horse' != Horse)
    } is sat
    horseNoHaveToChange : {always Horse' = Horse} is sat
    //var sig, has var relation to non-var sig
    dogCanAlwaysChange : {always Dog' != Dog} is sat
    dogCanSometimesChange : {
        (not always Dog' = Dog) and (not always Dog' != Dog)
    } is sat
    dogNoHaveToChange : {always Dog' = Dog} is sat
    //var sig, has non-var relation to non-var sig
    frogCanAlwaysChange : {always Frog' != Frog} is sat
    frogCanSometimesChange : {
        (not always Frog' = Frog) and (not always Frog' != Frog)
    } is sat
    frogNoHaveToChange : {always Frog' = Frog} is sat
    //var sig, referenced by var relation from non-var sig
    chinchillaCanAlwaysChange : {always Chinchilla' != Chinchilla} is sat
    chinchillaCanSometimesChange : {
        (not always Chinchilla' = Chinchilla) and (not always Chinchilla' != Chinchilla)
    } is sat
    chinchillaNoHaveToChange : {always Chinchilla' = Chinchilla} is sat
    //var sig, referenced by non-var relation from non-var sig
    rhinoCanAlwaysChange : {always Rhino' != Rhino} is sat
    rhinoCanSometimesChange : {
        (not always Rhino' = Rhino) and (not always Rhino' != Rhino)
    } is sat
    rhinoNoHaveToChange : {always Rhino' = Rhino} is sat
}
