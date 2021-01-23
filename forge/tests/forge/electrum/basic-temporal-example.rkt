#lang forge

option problem_type temporal
option verbose 0
option min_tracelength 3
--option solver MiniSatProver
--option core_minimization rce
--option logtranslation 1
sig A {}
sig Node { edges: set Node }
--run {some Node & A} for 3 Node

test expect {
    nodeAndADontIntersect : {some Node & A} is unsat
}

