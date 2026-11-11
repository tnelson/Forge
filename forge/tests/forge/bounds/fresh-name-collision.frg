#lang forge

option run_sterling off
option verbose 0

sig A {}
sig A1 {}

/** Regression test: the 11th atom of "A" should not be confused with the first atom of "A1". */
test expect {
    noOverlap: { some A & A1 } for 11 A, 1 A1 is unsat
    noOverlapExact: { some A & A1 } for exactly 11 A, exactly 1 A1 is unsat
}

/** Regression test: generated names cannot re-use user-provided atom names. */
sig B {}
sig B1 {}
test expect {
    userNameNotReused: { some B & B1 } for 11 B for { B1 = `B10 } is unsat
    userDollarNameNotReused: { some B & B1 } for 11 B for { B1 = `B$10 } is unsat
}
