#lang forge/bsl

sig Person {age: one Int}
one sig Nim extends Person {}

test expect {
    should_error: {reachable[age, Nim, Nim]} is sat
}