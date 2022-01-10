#lang forge/bsl
option verbose 0

sig Node {
    next: one Node
}

one sig A, B extends Node {}

pred leftjoin {
    some A.next
    some A.next.next.next
}

test expect {
  canRun: {leftjoin} is sat
}
