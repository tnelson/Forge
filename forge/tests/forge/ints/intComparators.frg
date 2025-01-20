#lang forge

option run_sterling off


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
    equal : EQ is checked
    greaterThan : GT is checked
    lessThan : LT is checked
    greaterThanEqual : GTE is checked
    lessThanEqual : LTE is checked

    notEqual : NEQ is checked
    notGreaterThan : NGT is checked
    notLessThan : NLT is checked
    notGreaterThanEqual : NGTE is checked
    notLessThanEqual : NLTE is checked
}
