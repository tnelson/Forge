#lang forge/bsl
option run_sterling off

option verbose 0

sig Node {
    next: one Node,
    field: pfunc Node -> Node
}

one sig A, B extends Node {}

pred leftjoin {
    -- we can always use multiplicity on a _sig_ in Froglet, whether or not it's "one".
    some Node  
    some A
    
    some A.next
    some A.next.next.next
    some A.field[A]
    some a: Node| some a.next
    all a: Node | some a.field[a]
    all a: Node | some a.field[A]
}

test expect {
  canRun: {leftjoin} is sat
}
