#lang forge/temporal 

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
    isUnsat: {init and always step} for 2 Int is forge_error
}