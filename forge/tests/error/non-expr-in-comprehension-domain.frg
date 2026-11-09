#lang forge
option verbose 0
sig Node { edges: set Node }
test expect {
    should_error: {
        #{n1,n2: (Node = Node) | some n1.edges} = 1
    } is sat
} 
