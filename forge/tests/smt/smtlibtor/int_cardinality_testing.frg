#lang forge 
option backend smtlibtor
one sig Helper { x, y: one Int }
test expect {
    emptyInt: {#Int = 0} is unsat
    singletonInt: {#Int = 1} is sat
    pairInt: {#Int = 2} is sat
    trioInt: {#Int = 3} is sat
}