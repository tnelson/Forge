#lang forge

/*
  Example puzzle model:

  Alice supervises Bob
  Bob supervises Charlie
  Alice went to Brown
  Charlie did not go to Brown
  
  Does a Brown graduate supervise a non-Brown graduate?
*/

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

--known2 : run known for 3 Person, 2 School
--known3 : run known for 3 Person, 3 School
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
--puzzle : run {known nobody} for exactly 3 Person, exactly 2 School
test expect {
  puzzleYes: {known nobody} for exactly 3 Person, exactly 2 School is unsat
}
