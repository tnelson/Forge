#lang forge

sig Node {edges: set Node}

pred irreflexive {    
    -- all n: Node | n not in n.^edges
    not (some iden & edges) -- "no" isn't handled correctly by evaluation TODO
}