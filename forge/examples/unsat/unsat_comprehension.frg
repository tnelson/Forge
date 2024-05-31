#lang forge/temporal 

option solver MiniSatProver
option core_minimization rce
option logtranslation 1
option coregranularity 1
option verbose 5

sig Node {edges: set Node}

run {
    no Node
    some {
        n1: Node | some n1
    }
}

