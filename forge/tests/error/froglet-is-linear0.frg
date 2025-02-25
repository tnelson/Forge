#lang forge/froglet
option run_sterling off
option verbose 0

sig Node {
    next: lone Node
}

// Attempt to use a binding expression in a predicate, rather than inst/example
pred ppred {
    next is linear
}
run {ppred}
