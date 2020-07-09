#lang forge

abstract sig Char {}

one sig A extends Char {}
one sig B extends Char {}
one sig C extends Char {}
one sig D extends Char {}
one sig E extends Char {}
one sig F extends Char {}
one sig G extends Char {}

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

fun first[r: set Int -> univ]: univ {
    r[sing[0]]
}

fun last[r: set Int -> univ]: univ {
    r[sing[subtract[#r, 1]]]
}

fun indsOf[r: set Int -> univ, e: set univ]: set Int {
    r.e
}

fun idxOf[r: set Int -> univ, e: set univ]: Int {
    min[r.e]
}

fun lastIdxOf[r: set Int -> univ, e: set univ]: Int {
    max[r.e]
}

fun elems[r: set Int -> univ]: set r[Int] {
    r[Int]
}

fun inds[r: set Int -> univ]: set Int {
    r.univ
}

pred isEmpty[r: set Int -> univ] {
    no r
}

pred hasDups[r: set Int -> univ] {
    some e: elems[r] | {
        #(indsOf[r, e]) > 1
    }
}


/*
run { 
    some t: seqChar | {
        isSeqOf[t.r, Char]
        #t.r > 3
        one c: Char | { #indsOf[t.r, c] > 1 }
        idxOf[t.r, A] = 2
        #elems[t.r] = 5
        first[t.r] = B
    }
} for 6 Int, exactly 1 seqChar
*/
// not working with Int as codomain

// unique spec
// using chars

pred prop1[input: seqChar, output: seqChar] {
    all e: elems[output.r] | idxOf[output.r, e] = lastIdxOf[output.r, e]
}

pred prop2[input: seqChar, output: seqChar] {
    all e: elems[output.r] | e in elems[input.r]
}

pred prop3[input: seqChar, output: seqChar] {
    all i,j: inds[output.r] | sum[i] < sum[j] => {
        idxOf[input.r, output.r[i]] <= idxOf[input.r, output.r[j]]
    }
}

pred prop4[input: seqChar, output: seqChar] {
    all e: elems[input.r] | e in elems[output.r]
}

pred prop5[input: seqChar, output: seqChar] {
    #input.r >= #output.r
}

pred prop6[input: seqChar, output: seqChar] {
    all e: elems[output.r] | {
        #(indsOf[output.r, e]) <= #(indsOf[input.r, e])
    }
}

pred goodinput[input: seqChar] {
    #input.r >= 7
    all e: elems[input.r] | #(indsOf[input.r, e]) <= 3
}

run {
    some i, o: seqChar | {
        isSeqOf[i.r, Char]
        isSeqOf[o.r, Char]
        goodinput[i]
        hasDups[i.r]
        --some c: Char | { #(indsOf[i.r, c]) > 1 }
        prop1[i, o]
        prop2[i, o]
        prop3[i, o]
        prop4[i, o]
        prop5[i, o]
        prop6[i, o]
    }
} for 7 Char --for 6 Int, exactly 2 seqChar