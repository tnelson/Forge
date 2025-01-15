#lang forge/froglet
option run_sterling off

sig Node {
    next: lone Node
}

sig A {
    field: one Node
}

pred cycle {
    all n: Node | reachable[n.next, n, field]
}

test expect {{cycle} is sat}
