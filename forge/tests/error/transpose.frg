#lang forge/bsl

sig Node {
    next: lone Node
}
one sig A, B extends Node{}

pred err {
    some ~next
}

test expect{
    {err} is sat
}