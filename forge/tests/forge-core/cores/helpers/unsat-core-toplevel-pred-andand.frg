#lang forge

option solver MiniSatProver
option logtranslation 2
option coregranularity 2
option core_minimization rce
option run_sterling off

sig Node {edges: set Node}

pred foo {
    some n1, n2, n3: Node | {
        n1->n2 in edges
        n2->n1 in edges
        n3 not in n3.edges
    }
}

predRun: run {
    foo
    no edges
}
