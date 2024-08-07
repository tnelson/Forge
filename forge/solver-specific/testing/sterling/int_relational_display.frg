#lang forge

option backend smtlibtor
option verbose 2

sig Person {
    -- changed; seemed to not terminate otherwise? 
    parent : lone Person,
    age : one Int
}

-- Confirm that Sterling works in combination with unbounded Ints
run {
  all p: Person | {
    -- TODO: shouldn't this be an error if parent is "lone", not "one"? 
    -- Keeping it in as example + reminder
    p.age < p.parent.age
  }
  #Person.age = 5
  some disj p1, p2: Person | p1.age = p2.age

  -- TODO in Sterling:
  -- Make sure that Sterling isn't hard-limiting the set Int based on bitwidth 
  -- all p: Person | p.age > 10
}
-- ^ Bounds are ignored, so we cannot say something like "for exactly 6 Person" here.
-- Instead, added the p1.age = p2.age constraint.
