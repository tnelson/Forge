#lang forge

sig seqChar {
    r: set Int -> Char
}

pred isSeqOf[r: set Int -> univ, d: set univ] {
    r in Int -> univ
    r[Int] in d
    all i: r.univ | {
        sum[i] >= 0 =>
        lone r[i]
    }
    all e: r[Int] | some r.e
    all i: (r.univ - sing[0]) | { some r[sing[subtract[sum[i], 1]]] }
}

run {
    some i,o: seqChar | {
        isSeqOf[i.r, Char]
        isSeqOf[o.r, Char]
    }
}
