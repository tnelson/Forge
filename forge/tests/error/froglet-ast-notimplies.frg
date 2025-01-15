#lang forge/froglet
option run_sterling off

sig Node {
    next: lone Node
}
one sig A, B extends Node{}

pred err {
    A not implies B else C
}

