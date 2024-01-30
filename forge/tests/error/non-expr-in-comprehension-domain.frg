#lang forge
option verbose 0
sig Node { edges: set Node }
test expect {
    -- "Set-comprehension condition expected a formula. "(n.edges)"
    --should_error: {
    --    #{n: Node | n.edges}
    --} is sat
    
    -- "Set-comprehension variable domain needs arity = 1 "edges"
    --should_error: {
    --    #{n: edges | some n.edges}
    --} is sat

    -- Set-comprehension variable domain expected a singleton or relational expression "5"
    should_error: {
        #{n1,n2: 5 | some n1.edges}
    } is sat
} 
