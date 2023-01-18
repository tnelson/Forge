#lang forge

option run_sterling off


option verbose 0

sig A {}
sig B {}

test expect sigsPopulate {
    singleSig : { some A } is sat
    doubleSig : { some A some B } is sat
}

test expect sigProperties {
    sigsDisjoint : { no A & B } is theorem
    -- sigsSpanUniv : { univ = A + B + Int } is theorem -- CURRENTLY BUGGED
}

