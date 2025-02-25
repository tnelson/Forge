#lang forge/froglet
option run_sterling off
option verbose 0

sig Node {
  next: lone Node
}

sig A {
  field: one Node
}

pred cycle {
  all n: Node | reachable[n.next]
}
assert {cycle} is sat