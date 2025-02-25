#lang forge/froglet

open "internal_identifiers.frg"
// Expect "option run_sterling off" to be imported with this.
assert {} is sat

// Workaround (open needs to be at start, so to run this, move to top):
//open "internal_identifiers.frg" as internal
// option run_sterling off
// assert {some internalState} is sat
