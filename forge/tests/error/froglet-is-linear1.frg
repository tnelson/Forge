#lang forge/froglet
option run_sterling off

sig Node {
    next: lone Node
}

// Attempt to use "is linear" binding expression on a sig, not a field
check {} for { Node is linear }

