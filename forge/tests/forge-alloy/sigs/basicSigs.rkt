#lang forge

sig A {}
sig B {}

test expect sigsPopulate {
    singleSig : { some A } is sat
    doubleSig : { some A some B } is sat
}

test expect sigProperties {
    sigsDisjoint : { some A & B } is unsat
    -- sigsSpanUniv : { univ != A + B + Int } is unsat -- CURRENTLY BUGGED
}

