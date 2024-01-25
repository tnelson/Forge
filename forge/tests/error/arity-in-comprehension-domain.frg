#lang forge
option verbose 0
sig Node { edges: set Node }
test expect {    
    -- "Set-comprehension variable domain needs arity = 1 "edges"
    should_error: {
       #{n: edges | some n.edges}
    } is sat

} 
