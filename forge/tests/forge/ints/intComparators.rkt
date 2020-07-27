#lang forge

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
    equal : {not EQ} is unsat
    greaterThan : {not GT} is unsat
    lessThan : {not LT} is unsat
    greaterThanEqual : {not GTE} is unsat
    lessThanEqual : {not LTE} is unsat

    notEqual : {not NEQ} is unsat
    notGreaterThan : {not NGT} is unsat
    notLessThan : {not NLT} is unsat
    notGreaterThanEqual : {not NGTE} is unsat
    notLessThanEqual : {not NLTE} is unsat
}