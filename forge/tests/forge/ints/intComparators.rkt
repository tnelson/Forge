#lang forge

option verbose 0

-- > < =

pred EQ {
    all i1, i2: Int |
        sum[i1] = sum[i2] iff i1 = i2
}

pred GT {
    all i1, i2: Int |
        sum[i1] > sum[i2] iff i1 in i2.^succ
}

pred LT {
    all i1, i2: Int |
        sum[i1] < sum[i2] iff i1 in ^succ.i2
}

-- >= <= 

pred GTE {
    all i1, i2: Int |
        sum[i1] >= sum[i2] iff i1 in i2.*succ
}

pred LTE {
    all i1, i2: Int |
        sum[i1] <= sum[i2] iff i1 in *succ.i2
}

-- != !> !< !>= !<=

pred NEQ {
    all i1, i2: Int |
        sum[i1] != sum[i2] iff not (sum[i1] = sum[i2])
}

pred NGT {
    all i1, i2: Int |
        sum[i1] !> sum[i2] iff not (sum[i1] > sum[i2])
}

pred NLT {
    all i1, i2: Int |
        sum[i1] !< sum[i2] iff not (sum[i1] < sum[i2])
}

pred NGTE {
    all i1, i2: Int |
        sum[i1] !>= sum[i2] iff not (sum[i1] >= sum[i2])
}

pred NLTE {
    all i1, i2: Int |
        sum[i1] !<= sum[i2] iff not (sum[i1] <= sum[i2])
}

test expect IntComparators {
    equal : EQ is theorem
    greaterThan : GT is theorem
    lessThan : LT is theorem
    greaterThanEqual : GTE is theorem
    lessThanEqual : LTE is theorem

    notEqual : NEQ is theorem
    notGreaterThan : NGT is theorem
    notLessThan : NLT is theorem
    notGreaterThanEqual : NGTE is theorem
    notLessThanEqual : NLTE is theorem
}
