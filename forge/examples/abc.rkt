#lang forge

-- TODO All sigs are abstract seems to be assumed still.

sig School {}
one sig Brown extends School {}
one sig Harvale extends School {}

sig Person {
    supervises: lone Person,
    school: one School
    }
one sig Alice extends Person {}
one sig Bob extends Person {}
one sig Charlie extends Person{}

pred known {
    supervises = Alice->Bob + Bob->Charlie
    Alice->Brown in school
    Charlie->Brown not in school
}

--known : run known for exactly 3 Person, exactly 2 School

-- TODO bounds only correct for "exact", hence all run commands except this one changed to "exactly"
--known : run known for 3 Person, 3 School
--------------------------------------------------------

pred somebody {
    some p: Person | some s: Person | {
        p.school != Brown
        s.school = Brown
        p in s.supervises
    }
}
pred nobody { not somebody }

-- Reverse!
--revpuzzle : run {known somebody} for exactly 3 Person, exactly 2 School
-- Actual puzzle (expect unsat)
puzzle : run {known nobody} for exactly 3 Person, exactly 2 School
