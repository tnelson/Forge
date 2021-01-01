#lang forge

sig Node {edges: set Node}

--option verbose 2

pred irreflexive {    
    --all n: Node | n not in n.^edges
    not (some iden & edges) -- "no" isn't handled correctly by evaluation TODO
}

--run {}