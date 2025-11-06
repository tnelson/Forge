#lang forge
option run_sterling off
option verbose 0 
sig Person { age: one Int }
one sig Solution { people: pfunc Int -> Person }
lone sig Light {}

/** Regression test for join-associativity parser bug. 
     Previously, this needed to be expressed as (Solution.people[0]).age. 
     Need to also check semantics. */
assert { Solution.people[0].age = 0 } is sat

test expect {
  { Solution.people[0].age = 0 
    iff
    ((Solution.people)[0]).age = 0} is checked
} 

////////////////////////////////////////////////////////////////

// Not yet fixed
/** Regression test for quantifier-precedence parser bug. 
     Previously, the quantified sub-expression needed parentheses:
     some Solution and (some i: Int | no Solution.people[i]) */
//assert { some Solution and some i: Int | no Solution.people[i]} is sat

////////// Test for quantifier semantics after parse. //////////

// This doesn't allow anybody to have a positive age if the light is off.
pred formulation_1 { (some p: Person | p.age > 0) implies (some Light) }

// This allows someone with a positive age even if the light is off.
pred formulation_2 { some p: Person | (p.age > 0 implies some Light) }

// Which does the version without parens parse to? Version 2. 
pred formulation_Q { some p: Person | p.age >0 implies some Light }
test expect {
  same_Q_2: { formulation_2 iff formulation_Q } is checked
}
// So Alloy's parsing disagrees with Enderton, who says that (paraphrasing)
// "...all and some apply to as little as possible. 
//  ...   all x | A => B is not all x | (A => B)"