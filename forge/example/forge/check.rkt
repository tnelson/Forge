#lang forge

sig Node {
    edges: set Node
}

pred Acyclic {
    no iden & ^edges
}

pred Irreflexive {
    no iden & edges
}

check {Acyclic implies Irreflexive}
check {Irreflexive implies Acyclic}
