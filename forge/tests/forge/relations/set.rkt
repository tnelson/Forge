#lang forge

sig Node {
    edge: set Node
}

test expect One {
    negative: { } is sat
}
