#lang forge
option verbose 0 

sig Parent {}
one sig A, B extends Parent {}

test expect {
    -- Having no bound for A or B child sigs
    -- Should still be SAT, or better yet, an error: how should Forge
    -- decide *which* of the atoms `A or `B should populate the singleton sigs?
    -- Enforce the sig definition explicitly to remove dependency
    shouldFailWithError: {one A and one B}     
    for { Parent = `A + `B } 
    is sat
}
