#lang forge

option run_sterling off


option verbose 0

sig Node {
    edge: one Node
}

test expect One {
    positive: { all n: Node | one n.edge } is theorem
    negative: { some n: Node | one n.edge } is sat
}
