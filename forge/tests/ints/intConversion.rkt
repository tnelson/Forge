#lang forge

option verbosity 5

sig A {
    iset: set Int,
    j: one Int
}

expect conversion {
    sum_sing_valid: { sum[sing[#A]] != #A } is unsat
    sing_sum_invalid: { sing[sum[A.iset]] != A.iset } is sat
    onesum_valid: { sing[sum[A.j]] != A.j } for exactly 1 A is unsat    
    onesum_invalid: { sing[sum[A.j]] != A.j } for exactly 2 A is sat
}

test expect operators {
    gt1: { some a1, a2: A | sum[a1.j] > sum[a2.j] } for exactly 2 A is sat
    gt2: { some a1 : A |    sum[a1.j] > sum[a1.j] } for exactly 2 A is unsat
    lt1: { some a1, a2: A | sum[a1.j] < sum[a2.j] } for exactly 2 A is sat
    gte1: { some a1, a2: A | sum[a1.j] >= sum[a2.j] } for exactly 2 A is sat
    lte1: { some a1, a2: A | sum[a1.j] <= sum[a2.j] } for exactly 2 A is sat

// TODO: exact insts, int bounds wraparound

    plus1:  { some i1, i2, i3: Int | add[sum[i1], sum[i2]] = sum[i3] } for exactly 0 A is sat
    minus1: { some i1, i2, i3: Int | subtract[sum[i1], sum[i2]] = sum[i3] } for exactly 0 A is sat
    mult1: { some i1, i2, i3: Int | multiply[sum[i1], sum[i2]] = sum[i3] } for exactly 0 A is sat
    div1: { some i1, i2, i3: Int | divide[sum[i1], sum[i2]] = sum[i3] } for exactly 0 A is sat

}



// Sterling seems to be throwing an error for some of these, too

expect unexpected {
    onesum_invalid1: { sing[sum[A.j]] != A.j } for 1 A is sat
    --rem1: { some i1, i2, i3: Int | remainder[sum[i1], sum[i2]] = sum[i3] } for exactly 0 A is sat

 -- abs1: { some i1, i2: Int | abs[sum[i1]] = sum[i2] } for exactly 0 A is sat -- identifier?
   -- sign1: { some i1 | sign[sum[i1]] = sum[i2] } for exactly 0 A is sat  -- what is the type of sign?

  -- Alloy produces "unsat" for both of these (0-variable trivial) with and without the sum operators
  divzero: { some i1, i3: Int | divide[sum[i1], 0] = sum[i3] } for exactly 0 A is unsat
 
  div2: { some i1: Int | divide[sum[i1], sum[i1]] != 1 } for exactly 0 A is unsat
}