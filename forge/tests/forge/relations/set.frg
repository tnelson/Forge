#lang forge

option run_sterling off


option verbose 0

sig Node {
    edge: set Node
}

test expect One {
    negative: { } is sat
}
