#lang forge

option no_overflow true // disallow integer overflow

sig Pigeon {location: one Pigeonhole}
sig Pigeonhole {}

pred some_roommates {
    some disj p1, p2: Pigeon | p1.location = p2.location
}

// This fails. Since the solver has yielded unsat, Sterling shouldn't open.
example threePigeons is {not some_roommates} for {
    Pigeon = `Pigeon0 + `Pigeon1 + `Pigeon2
    Pigeonhole = `Pigeonhole0 + `Pigeonhole1
    location = `Pigeon0 -> `Pigeonhole0 +
               `Pigeon1 -> `Pigeonhole0 +
               `Pigeon2 -> `Pigeonhole1 
}
