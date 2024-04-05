#lang forge/temporal 

option solver MiniSatProver
option core_minimization rce
option logtranslation 1
option coregranularity 1

sig Node {edges: set Node}

test expect {
    { 
        no Node
        some {n1: Node | some n1}
    } is unsat
}

