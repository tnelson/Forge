#lang forge/froglet

sig Person {age: one Int}
one sig Nim extends Person {}

test expect {
    should_error: {reachable[Nim, Nim, Nim]} is sat
}