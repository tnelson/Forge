#lang forge/temporal 

/*
  As of May 2024, this model now gives an error, rather than producing
  unsat. (The file previously tested behavior when unsat came from 
  lack of a "var" annotation on a field that needed to be variable.)
*/

option solver MiniSatProver
option core_minimization rce
option logtranslation 1
option coregranularity 1

one sig World {
    -- Forgot "var"!
    counter: one Int 
}

pred init { World.counter = 0 }
pred step { World.counter' = subtract[World.counter, -1]}
test expect {
    isUnsat: {init and always step} for 2 Int is forge_error // unsat
}