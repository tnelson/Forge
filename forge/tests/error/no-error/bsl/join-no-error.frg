#lang forge/bsl
option verbose 0

sig Node {
    next: one Node,
    field: pfunc Node -> Node
}

one sig A, B extends Node {}

pred leftjoin {
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
