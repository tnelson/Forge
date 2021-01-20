#lang forge

option verbose 0

-- sing      int -> set

pred Sing {
    no succ.(sing[-8])
    sing[-8].succ = sing[-7]
    sing[-7].succ = sing[-6]
    sing[-6].succ = sing[-5]
    sing[-5].succ = sing[-4]
    sing[-4].succ = sing[-3]
    sing[-3].succ = sing[-2]
    sing[-2].succ = sing[-1]
    sing[-1].succ = sing[0]
    sing[0].succ = sing[1]
    sing[1].succ = sing[2]
    sing[2].succ = sing[3]
    sing[3].succ = sing[4]
    sing[4].succ = sing[5]
    sing[5].succ = sing[6]
    sing[6].succ = sing[7]
    no sing[7].succ
}


abstract sig IntSet {
    ints: set Int
}
one sig S1 extends IntSet {}
one sig S2 extends IntSet {}
one sig S3 extends IntSet {}
one sig S4 extends IntSet {}
one sig S5 extends IntSet {}

-- sum       set -> int
-- sum-quant set -> int

inst SumInst {
    ints = -- S10->() +
           S20->(6) +
           S30->(1 + 2 + 3) +
           S40->(-5 + -1 + 3) +
           S50->(7 + 1)
}

pred Sum {
    all i: Int |
        i = sing[sum[i]]

    sum[S1.ints] = 0
    sum[S2.ints] = 6
    sum[S3.ints] = 6
    sum[S4.ints] = -3
    sum[S5.ints] = 8
}

pred SumQuant {
    (sum S: IntSet | min[S.ints]) = 3
    (sum S: IntSet | max[S.ints]) = 19
    (sum S: IntSet | #S.ints) = 9

    -- This also checks that sum works with duplicates
    (sum S: IntSet | sum i: S.ints | sum[i]) = 1

    (sum S: IntSet | sum i: S.ints | multiply[sum[i], sum[i]]) = 135
}


-- card      set -> int

inst CardInst {
    ints = -- S10->() +
           S20->(-5) +
           S30->(-3 + 0) +
           S40->(-8 + 7 + 1) + 
           S50->(4 + 3 + 2 + 1)
}

pred Card {
    all i: Int |
        card[i] = 1

    card[S1.ints] = 0
    card[S2.ints] = 1
    card[S3.ints] = 2
    card[S4.ints] = 3
    card[S5.ints] = 4

    all S: IntSet |
        card[S.ints] = #S.ints
}


-- max, min  set -> int

inst MaxMinInst {
    ints = S10->(0) +
           S20->(0 + 1) +
           S30->(-5 + -2 + 0 + 4) +
           S40->(7) +
           S50->(-8 + -7 + -6 + -5 + -4 + -3 + -2 + -1 +
                 0 + 1 + 2 + 3 + 4 + 5 + 6 + 7)
}

pred MaxMin {
    min[S1.ints] = 0
    max[S1.ints] = 0

    min[S2.ints] = 0
    max[S2.ints] = 1

    min[S3.ints] = -5
    max[S3.ints] = 4

    min[S4.ints] = 7
    max[S4.ints] = 7

    min[S5.ints] = -8
    max[S5.ints] = 7
}


test expect {
    sings : Sing for 4 Int, 5 IntSet is theorem
    sums : Sum for 5 IntSet for SumInst is theorem
    sumQuants : SumQuant for 5 IntSet for SumInst is theorem
    cards : Card for 5 IntSet for CardInst is theorem
    maxMins : MaxMin for 5 IntSet for MaxMinInst is theorem
}
