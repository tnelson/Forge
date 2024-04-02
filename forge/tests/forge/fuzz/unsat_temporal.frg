#lang forge/temporal 
option min_tracelength 3
option solver MiniSatProver
option core_minimization rce
option logtranslation 1

option max_tracelength 16

one sig World {
    var counter: one Int 
}

pred init { World.counter = 0 }
pred step { World.counter' = add[World.counter, 1]}
test expect {
    isUnsat: {init and always step and World.counter = 1} is unsat
}