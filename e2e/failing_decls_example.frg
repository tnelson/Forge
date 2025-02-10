#lang forge

option no_overflow true // disallow integer overflow

sig Pigeon {location: one Pigeonhole}
sig Pigeonhole {}

pred some_roommates {
    some disj p1, p2: Pigeon | p1.location = p2.location
}

// This fails because the instance given violates the type definitions
// The solver yields unsat, so Sterling should not open.
example threePigeons is {some_roommates} for {
    Pigeon = `Pigeon0 + `Pigeon1 + `Pigeon2
    Pigeonhole = `Pigeonhole0 + `Pigeonhole1
    // This violates the type declarations:
    location = `Pigeon0 -> (`Pigeonhole0 + `Pigeonhole1) +
               `Pigeon1 -> `Pigeonhole0 +
               `Pigeon2 -> `Pigeonhole0 
}
