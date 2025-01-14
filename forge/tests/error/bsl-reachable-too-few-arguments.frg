#lang forge/froglet
option run_sterling off


sig Node {
    next: lone Node
}

pred cycle {
    all n: Node | reachable[n.next, n]
}

test expect {{cycle} is sat}
