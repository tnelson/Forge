#lang forge

option no_overflow true // disallow integer overflow

sig Pigeon {location: one Pigeonhole}
sig Pigeonhole {}

pred some_roommates {
    some disj p1, p2: Pigeon | p1.location = p2.location
}
test expect { 
  -- This is satisfiable, and so should appear in the run menu.
  not_vacuous: {#Pigeon > #Pigeonhole} is sat 
  -- This is unsatisfiable, and so wouldn't be useful to visualize.
  should_be_unsat: {Pigeon != Pigeon} is unsat 
}

see_principle_1: run {} for exactly 5 Pigeon, exactly 4 Pigeonhole
see_principle_2: run {} for exactly 4 Pigeon, exactly 3 Pigeonhole
