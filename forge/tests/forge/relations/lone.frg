#lang forge

option run_sterling off


option verbose 0

sig Node {
    edge: lone Node
}

test expect Lone {
    positive: { all n: Node | lone n.edge } is theorem
    negativeNo: { some n: Node | no n.edge } is sat
    negativeOne: { some n: Node | one n.edge } is sat
}
