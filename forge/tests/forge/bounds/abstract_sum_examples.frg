#lang forge
option run_sterling off
option verbose 0

/*
  How should Forge resolve the situation where an abstract sig A has no scope, 
  but all (or all but one) of its children do (either user-provided, via
  `one`/`lone`, or otherwise)? 

  Alloy's behavior is: _sum_ over the max-scopes of all A's children, recursively applied. 
  
  Separately, if the abstract parent and all but one of its children are scoped, then 
  that child gets the remainder.
  
  Forge will echo that as of the January 2027 version.
*/
 
/* 
  ---- Abstract parent has no declared scope; immediate children do. ----
  E1_Abs (abs)
  ├─ E1_Abs_KidA [3]
  └─ E1_Abs_KidB [3]          sum = 6
*/
abstract sig E1_Abs {}
sig E1_Abs_KidA, E1_Abs_KidB extends E1_Abs {}
test expect ex1 {
  ex1_sum_exceeds_default: { #E1_Abs_KidA = 3 and #E1_Abs_KidB = 3 } 
    for 3 E1_Abs_KidA, 3 E1_Abs_KidB is sat
  ex1_sum_below_default: { #E1_Abs_KidA = 1 and #E1_Abs_KidB = 1 } 
    for 1 E1_Abs_KidA, 1 E1_Abs_KidB is sat
  ex1_inst_cardinality: { #E1_Abs_KidA = 3 and #E1_Abs_KidB = 3 } 
    for { #E1_Abs_KidA <= 3  #E1_Abs_KidB <= 3 } is sat
}

/* ---- Recursive: an abstract parent of an abstract child. ----
  E2_Abs (abs)
  ├─ E2_Abs_Kid [2]
  ├─ E2_Abs_Abs (abs)          sum = 2
  │  ├─ E2_Abs_Abs_OneA (one)
  │  └─ E2_Abs_Abs_OneB (one)
  └─ E2_Abs_One (one)         sum = 2 + 2 + 1 = 5
*/
abstract sig E2_Abs {}
sig E2_Abs_Kid extends E2_Abs {}
abstract sig E2_Abs_Abs extends E2_Abs {}
one sig E2_Abs_Abs_OneA, E2_Abs_Abs_OneB extends E2_Abs_Abs {}
one sig E2_Abs_One extends E2_Abs {}

test expect ex2 {
  ex2_recursive_one_sigs: { #E2_Abs_Kid = 2 } for 2 E2_Abs_Kid is sat
  ex2_overall_scope: { #E2_Abs_Kid = 2 } for 1 but 2 E2_Abs_Kid is sat
}

/* ---- Recursive variant: two levels of declared scopes ----
  E3_Abs (abs)
  ├─ E3_Abs_Abs (abs)          sum = 3
  │  ├─ E3_Abs_Abs_KidA [2]
  │  └─ E3_Abs_Abs_KidB [1]
  └─ E3_Abs_Kid [2]           sum = 3 + 2 = 5
*/
abstract sig E3_Abs {}
abstract sig E3_Abs_Abs extends E3_Abs {}
sig E3_Abs_Abs_KidA, E3_Abs_Abs_KidB extends E3_Abs_Abs {}
sig E3_Abs_Kid extends E3_Abs {}

test expect ex3 {
  ex3_recursive_declared: { 
    #E3_Abs_Abs_KidA = 2 and #E3_Abs_Abs_KidB = 1 and #E3_Abs_Kid = 2 
  } for 2 E3_Abs_Abs_KidA, 1 E3_Abs_Abs_KidB, 2 E3_Abs_Kid is sat
}

/* ---- A `lone` child contributes 1 to the sum, as `one` does. ----
  E4_Abs (abs)
  ├─ E4_Abs_Kid [4]
  └─ E4_Abs_Lone (lone)       sum = 4 + 1 = 5
*/
abstract sig E4_Abs {}
sig E4_Abs_Kid extends E4_Abs {}
lone sig E4_Abs_Lone extends E4_Abs {}

test expect ex4 {
  ex4_lone_child: { #E4_Abs_Kid = 4 and some E4_Abs_Lone } 
    for 4 E4_Abs_Kid is sat
}

/* ---- If not all children are scoped, the parent keeps the default. ----
  E5_Abs (abs)
  ├─ E5_Abs_KidA [3]
  └─ E5_Abs_KidB              no sum: child unscoped

  THIS IS NOT ALLOY 6'S BEHAVIOR! Alloy would throw an error in these cases:
    (a) there is no general `for N` given;
  - (b) there is at least one provided scope; 
  - (c) at least one top-level sig lacks a scope even after processing `one` and `lone` 
    sigs and recursively resolving missing scopes. 
  In contrast, Forge gives the scope-less sig its default.

  This situation produces a warning in Forge. 
  It may become an error in a future release.
*/
abstract sig E5_Abs {}
sig E5_Abs_KidA, E5_Abs_KidB extends E5_Abs {}

test expect ex5 {
  ex5_some_unscoped_sat:   { #E5_Abs_KidA = 3 and #E5_Abs_KidB = 1 } 
    for 3 E5_Abs_KidA is sat
  ex5_some_unscoped_unsat: { #E5_Abs_KidA = 3 and #E5_Abs_KidB = 2 } 
    for 3 E5_Abs_KidA is unsat
}

/* --- If an abstract sig is explicitly scoped, as are all but one of its children,
  the unscoped child receives the difference as its scope. 
  E6_Abs (abs) [5]
  ├─ E6_Abs_KidA [3]
  └─ E6_Abs_KidB              derived: 5 - 3 = 2
*/
abstract sig E6_Abs {}
sig E6_Abs_KidA, E6_Abs_KidB extends E6_Abs {}

test expect ex6 {
  ex6_difference_sat:   { #E6_Abs_KidB = 2 } for 5 E6_Abs, 3 E6_Abs_KidA is sat
  ex6_difference_unsat: { #E6_Abs_KidB = 3 } for 5 E6_Abs, 3 E6_Abs_KidA is unsat
}
