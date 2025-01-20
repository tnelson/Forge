#lang forge

option run_sterling off


option verbose 0

-- abs, sign

pred Abs {
    abs[-8] = -8
    abs[-7] = 7
    abs[-6] = 6
    abs[-5] = 5
    abs[-4] = 4
    abs[-3] = 3
    abs[-2] = 2
    abs[-1] = 1
    abs[0] = 0
    abs[1] = 1
    abs[2] = 2
    abs[3] = 3
    abs[4] = 4
    abs[5] = 5
    abs[6] = 6
    abs[7] = 7
}

pred Sign {
    sign[-8] = -1
    sign[-7] = -1
    sign[-6] = -1
    sign[-5] = -1
    sign[-4] = -1
    sign[-3] = -1
    sign[-2] = -1
    sign[-1] = -1
    sign[0] = 0
    sign[1] = 1
    sign[2] = 1
    sign[3] = 1
    sign[4] = 1
    sign[5] = 1
    sign[6] = 1
    sign[7] = 1
}

-- add, subtract, multiply, divide, remainder

pred Add {
    all i: Int |
        add[sum[i], 0] = sum[i]

    let succ2 = succ + (Int - succ.Int)->(Int - Int.succ) |
        all i1, i2: Int |
            sing[add[sum[i1], sum[i2.succ2]]] = sing[add[sum[i1], sum[i2]]].succ2
}

pred Multiply {
    all i: Int |
        multiply[sum[i], 0] = 0

    let succ2 = succ + (Int - succ.Int)->(Int - Int.succ) |
        all i1, i2: Int |
            multiply[sum[i1], sum[i2.succ2]] = add[sum[i1], multiply[sum[i1], sum[i2]]]
}

pred Subtract {
    all i1, i2: Int |
        add[subtract[sum[i1], sum[i2]], sum[i2]] = sum[i1]
}

pred DivideRemainder {
    all i1: Int, i2: (Int - sing[0]) | let x = sum[i1], y = sum[i2] | 
        let q = divide[x, y], r = remainder[x, y] | {
            sign[r] = sign[x] or sign[r] = 0
            abs[r] < abs[y] or y = -8
            x = add[multiply[y, q], r]
        }
}

test expect {
    absTest : Abs for 4 Int is checked
    signTest : Sign for 4 Int is checked
    addTest : Add is checked
    subtractTest : Subtract is checked
    multiplyTest : Multiply is checked
    divideRemainderTest : DivideRemainder is checked
}
