#lang forge

sig Node {edges: set Node}

pred irreflexive {    
    --all n: Node | n not in n.^edges
    no iden & edges   
}
