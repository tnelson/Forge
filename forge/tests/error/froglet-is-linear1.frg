#lang forge/froglet
option run_sterling off

sig Node {
    next: lone Node
}

check {} for { Node is linear }

