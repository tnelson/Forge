#lang forge

sig A {
    i: set Int
}

test expect foo {
    sum_sing_valid: { sum[sing[#A]] != #A } is unsat
--    sing_sum_invalid: { sing[sum[A.i]] != A.i } is sat
}
