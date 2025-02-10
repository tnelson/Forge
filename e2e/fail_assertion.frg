#lang forge

option no_overflow true // disallow integer overflow

sig Pigeon {location: one Pigeonhole}
sig Pigeonhole {}

pred some_roommates {
    some disj p1, p2: Pigeon | p1.location = p2.location
}
-- This should pass; we don't expect Sterling to open at all.
assert {#Pigeon > #Pigeonhole} is sufficient for some_roommates
-- This should fail; we expect Sterling to open with **only one command**,
-- and for the instance to auto-load. **The evaluator should be usable**.
assert {#Pigeon >= #Pigeonhole} is sufficient for some_roommates

