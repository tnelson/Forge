#lang forge/froglet

// Examples are instances, but they have a separate syntax macro in core.
// Make sure that example bounds are being processed correctly.

sig Person {
    partner: lone Person
}

pred symmetricSize6 {
    all p1,p2: Person | p1.partner = p2 iff p2.partner = p1
    some p: Person | some p.partner
    #Person = 6 -- default bitwidth = 4 --> [-8, 7], so can count to 6
}
pred symmetricSize2 {
    all p1,p2: Person | p1.partner = p2 iff p2.partner = p1
    some p: Person | some p.partner
    #Person = 2
}

example bigExample is {symmetricSize6} for {
    Person = `Person0 + `Person1 + `Person2 + `Person3 + `Person4 + `Person5
    partner = `Person0 -> `Person1 + `Person1 -> `Person0 + 
              `Person2 -> `Person5 + `Person5 -> `Person2 
}

example bigExample_indexing is {symmetricSize6} for {
    Person = `Person1 + `Person2 + `Person3 + `Person4 + `Person5 + `Person6
    partner = `Person1 -> `Person2 + `Person2 -> `Person1 + 
              `Person3 -> `Person6 + `Person6 -> `Person3 
}

example smallExample_indexing is {symmetricSize2} for {
    Person = `Person4 + `Person6
    partner = `Person4 -> `Person6 + `Person6 -> `Person4 
}

example smallExample_renaming is {symmetricSize2} for {
    Person = `P4 + `P6
    partner = `P4 -> `P6 + `P6 -> `P4 
}

-- Check Froglet syntax variant for example bounds
example smallExample_renaming_check_froglet_stx is {symmetricSize2} for {
    Person = (`P4 + `P6)
    partner = (`P4 , `P6) + `P6 -> `P4 
}

-- Check that Froglet syntax works in `inst`,
-- and that an inst can be used directly in example syntax
inst smallex_froglet {
    Person = (`P4 + `P6)
    partner = (`P4 , `P6) + `P6 -> `P4 
}
example smallExample_size2_indirect is {symmetricSize2} for smallex_froglet

inst piecewise {
    Person = `Alice + `Bob + `Charli
    `Alice.partner = `Bob
    `Bob.partner = `Alice
    no `Charli.partner
}
example checkPiecewise is {some disj p1,p2,p3: Person | p1.partner = p2 and p2.partner = p1 and no p3.partner}
  for piecewise                                
test expect {
  piecewise_partial: {
    some disj p1,p2,p3: Person | p1.partner = p2 and p2.partner = p1 and no p3.partner
  } for piecewise is checked
}
