#lang forge

pred isSeqOf[r1: set Int -> univ, d: set univ] {
    r1 in Int -> univ
    r1[Int] in d
    all i1: r1.univ | sum[i1] >= 0
    all i1: r1.univ | {
        sum[i1] >= 0 =>
        lone r1[i1]
    }
    all e: r1[Int] | some r1.e
    all i1: (r1.univ - sing[0]) | { some r1[sing[subtract[sum[i1], 1]]] }
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

abstract sig Char {}

one sig A extends Char {}
one sig B extends Char {}
one sig C extends Char {}
one sig D extends Char {}
one sig E extends Char {}
one sig F extends Char {}
one sig G extends Char {}
one sig H extends Char {}
one sig I extends Char {}
one sig J extends Char {}
one sig K extends Char {}
one sig L extends Char {}
one sig M extends Char {}
one sig N extends Char {}
one sig O extends Char {}
one sig P extends Char {}
one sig Q extends Char {}
one sig R extends Char {}
one sig S extends Char {}
one sig T extends Char {}
one sig U extends Char {}
one sig V extends Char {}
one sig W extends Char {}
one sig X extends Char {}
one sig Y extends Char {}
one sig Z extends Char {}
/* n is already used, also r as relation
one sig a extends Char {}
one sig b extends Char {}
one sig c extends Char {}
one sig d extends Char {}
one sig e extends Char {}
one sig f extends Char {}
one sig g extends Char {}
one sig h extends Char {}
one sig i extends Char {}
one sig j extends Char {}
one sig k extends Char {}
one sig l extends Char {}
one sig m extends Char {}
one sig n extends Char {}
one sig o extends Char {}
one sig p extends Char {}
one sig q extends Char {}
one sig r extends Char {}
one sig s extends Char {}
one sig t extends Char {}
one sig u extends Char {}
one sig v extends Char {}
one sig w extends Char {}
one sig x extends Char {}
one sig y extends Char {}
one sig z extends Char {}
one sig n1 extends Char {}
one sig n2 extends Char {}
one sig n3 extends Char {}
one sig n4 extends Char {}
one sig n5 extends Char {}
one sig n6 extends Char {}
one sig n7 extends Char {}
one sig n8 extends Char {}
one sig n9 extends Char {}
one sig n0 extends Char {}
*/

sig seqChar {
    r: set Int -> Char
}

// not working with Int as codomain

// unique spec
// using chars

-- to get interesting examples:

pred interesting[input: seqChar, output: seqChar] {
    #input.r > 6
    #output.r > 3
    hasDups[input.r]
}

-- to make sure in/out are seqs:

pred constrain[input: seqChar, output: seqChar] {
    isSeqOf[input.r, Char]
    isSeqOf[output.r, Char]
}

-- no duplicates in output
pred p1[input: seqChar, output: seqChar] {
    all e: elems[output.r] | idxOf[output.r, e] = lastIdxOf[output.r, e]
}

-- everything in output is in input
pred p2[input: seqChar, output: seqChar] {
    all e: elems[output.r] | e in elems[input.r]
}

-- same order
pred p3[input: seqChar, output: seqChar] {
    all i1,j: inds[output.r] | sum[i1] < sum[j] => {
        idxOf[input.r, output.r[i1]] <= idxOf[input.r, output.r[j]]
    }
}

-- everything in input is in output
pred p4[input: seqChar, output: seqChar] {
    all e: elems[input.r] | e in elems[output.r]
}

-- length of output <= length of input
pred p5[input: seqChar, output: seqChar] {
    #input.r >= #output.r
}

-- count of every unique val in output <= count of said value in input
pred p6[input: seqChar, output: seqChar] {
    all e: elems[output.r] | {
        #(indsOf[output.r, e]) <= #(indsOf[input.r, e])
    }
}

pred goodinput[input: seqChar] {
    #input.r >= 7
    all e: elems[input.r] | #(indsOf[input.r, e]) <= 3
}
/*
run {
    some i,o: seqChar | {
        goodinput[i]
        #o.r > 0
        hasDups[i.r]
        --some c: Char | { #(indsOf[i.r, c]) > 1 }
        p1[i, o]
        p2[i, o]
        p3[i, o]
        p4[i, o]
        p5[i, o]
        p6[i, o]
       -- p7[i, o]
    }
}-- for 7 Char, 4 Int, exactly 2 seqChar
*/