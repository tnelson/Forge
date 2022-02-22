#lang forge/bsl

sig Node {
    next: lone Node
}

sig A {
    field: one Node
}

pred cycle {
    all n: Node | reachable[next, n, next]
}

run {cycle}
