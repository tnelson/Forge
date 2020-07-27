#lang forge

option solver SAT4J

sig A {
    iset: set Int,
    j: one Int
}

test expect conversion {
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

    plus1:  { some i1, i2, i3: Int | add[sum[i1], sum[i2]] = sum[i3] } for exactly 0 A is sat
    minus1: { some i1, i2, i3: Int | subtract[sum[i1], sum[i2]] = sum[i3] } for exactly 0 A is sat
    mult1: { some i1, i2, i3: Int | multiply[sum[i1], sum[i2]] = sum[i3] } for exactly 0 A is sat
    div1: { some i1, i2, i3: Int | divide[sum[i1], sum[i2]] = sum[i3] } for exactly 0 A is sat
    div2: { some i: Int | divide[0, sum[i]] != 0 } for exactly 0 A is unsat

    abs1: { some i1: Int | all i2: Int | abs[sum[i1]] != sum[i2] } for exactly 0 A is unsat -- negative numbers exist
    abs2: { all i: Int | sum[i] >= 0 implies abs[sum[i]] = sum[i] } for exactly 0 A is sat  -- no effect on positives

    max1: { max[{x: Int | sum[x] > 0 and sum[x] < 3}] != 2} for exactly 0 A is unsat
    min1: { min[{x: Int | sum[x] > 0 and sum[x] < 3}] != 1} for exactly 0 A is unsat
    sign1: { sign[2] != 1 } for exactly 0 A is unsat
    sign2: { sign[-2] != -1 } for exactly 0 A is unsat
    sign3: { sign[2] = 1 } for exactly 0 A is sat
    sign4: { sign[-2] = -1 } for exactly 0 A is sat

    rem1: { some i1, i2, i3: Int | remainder[sum[i1], sum[i2]] = sum[i3] } for exactly 0 A is sat
    rem2: { remainder[5, 2] != 1 } for exactly 0 A is unsat
    rem2a: { remainder[5, 2] = 1 } for exactly 0 A is sat
    rem3: { remainder[0, 2] != 0 } for exactly 0 A is unsat
    rem3a: { remainder[0, 2] = 0 } for exactly 0 A is sat
    rem4: { remainder[-1, 2] != -1 } for exactly 0 A is unsat
    rem4a: { remainder[-1, 2] = -1 } for exactly 0 A is sat

    nonesum1: { sum[none] != 0 } for exactly 0 A is unsat
    nonesum2: { sum[none] = 0 } for exactly 0 A is sat    
    somesum1: { (some a1, a2, a3: A | a1.j = sing[-1] and a2.j = sing[2] and a3.j = sing[3]) sum[A.j] = 4 } for exactly 3 A is sat
    somesum2: { (some a1, a2, a3: A | a1.j = sing[-1] and a2.j = sing[2] and a3.j = sing[3]) sum[A.j] != 4 } for exactly 3 A is unsat
    
    somesumq1: { some a1, a2, a3: A | a1.j = sing[-1] and a2.j = sing[2] and a3.j = sing[3] (sum a : A | sum[a.j]) = 4 } for exactly 3 A is sat
    somesumq2: { some a1, a2, a3: A | a1.j = sing[-1] and a2.j = sing[2] and a3.j = sing[3] (sum a : A | sum[a.j]) != 4 } for exactly 3 A is unsat
    sumqdupes1: { all a: A | a.j = sing[1] (sum a : A | sum[a.j]) = 3 } for exactly 3 A is sat
    sumqdupes2: { all a: A | a.j = sing[1] (sum a : A | sum[a.j]) != 3 } for exactly 3 A is unsat
    someqwrap: { some a1, a2, a3: A | a1.j = sing[3] and a2.j = sing[3] and a3.j = sing[3] (sum a : A | sum[a.j]) = -8 } for exactly 3 A is sat

}

expect unexpected {
    -- Alloy produces "unsat" for this without the implies (0-variable trivial), with and without the sum operators
    -- but I suspect that's because of overflow protection. 
    divzero: { some i1, i3: Int | i1!=sing[0] and divide[sum[i1], 0] = sum[i3] } for exactly 0 A is unsat
}