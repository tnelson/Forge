#lang forge

sig Node {edges: set Node}

pred wheat hiddenPred {
    edges = ~edges
}

check { true } for { hiddenPred }
// Invalid bounds should not print the hidden pred

