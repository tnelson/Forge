#lang forge

sig Node {
    edge: one Node
}

test expect One {
    positive: { all n: Node | one n.edge } is theorem
    negative: { some n: Node | one n.edge } is sat
}
