#lang forge 

one sig Counter {
    n: one Int 
}

test expect {
    -- We are not trying to eliminate all overflow with this test. Rather,
    -- we want to give a useful error if the user has entered an int literal
    -- that does not fall into the range induced by the bitwidth.
    -- 5 Int ~= 2^5 integers ~= [-16, 15]
    should_error: { Counter.n > 20} for 5 Int is sat
}