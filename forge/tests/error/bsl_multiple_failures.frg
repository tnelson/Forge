#lang forge/bsl
option run_sterling off

option verbose 0

sig Node {
    next: one Node
}

one sig A, B extends Node {}

pred setcomp {
    #{n : Node | n.next = n} = #{n : Node | n.next = n}
}


test expect {
  t1: {!setcomp} is sat
  t2: {setcomp} is unsat
}