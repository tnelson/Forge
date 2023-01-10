#lang forge
option run_sterling off
option verbose 0

sig Person {}
sig State {
    age: func Person -> Int
}

test expect {
    -- Formally, this should be unsat. However, we should produce
    -- an error since the tuples given are ill-typed:
    --   age: State -> Person -> Int  not
    --        Person -> State -> Int
    shouldErrorBecauseIllTyped: {} for {
        Person = `P0
        State = `S0 + `S1 + `S2
        age = `P0 -> `S0 -> 1 +
              `P0 -> `S1 -> 2
    } is sat
}