#lang forge/bsl

sig Node {
    next: one Node
}

one sig A, B extends Node {}

pred setcomp {
    #{n : Node | n.next = n} = #{n : Node | n.next = n}
}

run {setcomp}