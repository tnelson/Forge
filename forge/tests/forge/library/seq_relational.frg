#lang forge
/*
  Some behaviors of the sequence library are relational by nature. This module tests them,
  using the same definitions as in the base seq.frg file.
*/

open "seq.frg"
option run_sterling off

test expect {
    indsOf_produces_all: {order implies (indsOf[Data.seq, A] = (0+2))} is theorem
    elems_check: {order implies (elems[Data.seq] = (A+B+C+D))} is theorem
    inds_check: {order implies (inds[Data.seq] = (0+1+2+3+4))} is theorem
}