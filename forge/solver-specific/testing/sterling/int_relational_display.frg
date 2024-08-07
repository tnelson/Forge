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
  #Person.age = 3
}
