#lang forge
/*
  Some behaviors of the sequence library are relational by nature. This module tests them,
  using the same definitions as in the base sequences.frg file.
*/

open "sequences.frg"
open util/sequences 

option run_sterling off

test expect {
    indsOf_produces_all: {order implies (indsOf[Data.seq, A] = (0+2))} is checked
    elems_check: {order implies (elems[Data.seq] = (A+B+C+D))} is checked
    inds_check: {order implies (inds[Data.seq] = (0+1+2+3+4))} is checked
}

/////////////////////////////

sig Element {}

one sig Wrapper {
    s: pfunc Int -> Element
}

test expect {
    isSeqTest: {
        isSeqOf[Wrapper.s, Element] implies {
            no Wrapper.s or min[Wrapper.s.Element] = 0
        }
    } is checked

    seqLastTest: {
        isSeqOf[Wrapper.s, Element] and some Wrapper.s implies 
          #(Wrapper.s) = add[1, #(seqRest[Wrapper.s])]
    } is checked
}