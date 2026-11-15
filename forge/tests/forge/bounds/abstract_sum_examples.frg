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
  P1 (abs)
  ├─ P1a [3]
  └─ P1b [3]        sum = 6
*/
abstract sig P1 {}
sig P1a, P1b extends P1 {}
test expect ex1 {
  ex1_sum_exceeds_default: { #P1a = 3 and #P1b = 3 } 
    for 3 P1a, 3 P1b is sat
  ex1_sum_below_default: { #P1a = 1 and #P1b = 1 } 
    for 1 P1a, 1 P1b is sat
  ex1_inst_cardinality: { #P1a = 3 and #P1b = 3 } 
    for { #P1a <= 3  #P1b <= 3 } is sat
}

/* ---- Recursive: an abstract parent of an abstract child. ----
  P2 (abs)
  ├─ P2a [2]
  ├─ P2b (abs)      sum = 2
  │  ├─ P2ba (one)
  │  └─ P2bb (one)
  └─ P2c (one)      sum = 2 + 2 + 1 = 5
*/
abstract sig P2 {}
sig P2a extends P2 {}
abstract sig P2b extends P2 {}
one sig P2ba, P2bb extends P2b {}
one sig P2c extends P2 {}

test expect ex2 {
  ex2_recursive_one_sigs: { #P2a = 2 } for 2 P2a is sat
  ex2_overall_scope: { #P2a = 2 } for 1 but 2 P2a is sat
}

/* ---- Recursive variant: two levels of declared scopes ----
  P3 (abs)
  ├─ P3a (abs)      sum = 3
  │  ├─ P3aa [2]
  │  └─ P3ab [1]
  └─ P3b [2]        sum = 3 + 2 = 5
*/
abstract sig P3 {}
abstract sig P3a extends P3 {}
sig P3aa, P3ab extends P3a {}
sig P3b extends P3 {}

test expect ex3 {
  ex3_recursive_declared: { 
    #P3aa = 2 and #P3ab = 1 and #P3b = 2 
  } for 2 P3aa, 1 P3ab, 2 P3b is sat
}

/* ---- A `lone` child contributes 1 to the sum, as `one` does. ----
  P4 (abs)
  ├─ P4a [4]
  └─ P4b (lone)     sum = 4 + 1 = 5
*/
abstract sig P4 {}
sig P4a extends P4 {}
lone sig P4b extends P4 {}

test expect ex4 {
  ex4_lone_child: { #P4a = 4 and some P4b } 
    for 4 P4a is sat
}

/* ---- If not all children are scoped, the parent keeps the default. ----
  P5 (abs)
  ├─ P5a [3]
  └─ P5b            no sum: child unscoped

  THIS IS NOT ALLOY 6'S BEHAVIOR! Alloy would throw an error in these cases:
    (a) there is no general `for N` given;
  - (b) there is at least one provided scope; 
  - (c) at least one top-level sig lacks a scope even after processing `one` and `lone` 
    sigs and recursively resolving missing scopes. 
  In contrast, Forge gives the scope-less sig its default.

  This situation produces a warning in Forge. 
  It may become an error in a future release.
*/
abstract sig P5 {}
sig P5a, P5b extends P5 {}

test expect ex5 {
  ex5_some_unscoped_sat:   { #P5a = 3 and #P5b = 1 } 
    for 3 P5a is sat
  ex5_some_unscoped_unsat: { #P5a = 3 and #P5b = 2 } 
    for 3 P5a is unsat
}

/* --- If an abstract sig is explicitly scoped, as are all but one of its children,
  the unscoped child receives the difference as its scope. 
  P6 (abs) [5]
  ├─ P6a [3]
  └─ P6b            derived: 5 - 3 = 2
*/
abstract sig P6 {}
sig P6a, P6b extends P6 {}

test expect ex6 {
  ex6_difference_sat:   { #P6b = 2 } for 5 P6, 3 P6a is sat
  ex6_difference_unsat: { #P6b = 3 } for 5 P6, 3 P6a is unsat
}

/* ---- Bounds-based minima and scopes, when consistent, should mix safely. ----
  sum:                      difference:
  P7 (abs)  sum = 6         P7 (abs) [6]
  ├─ P7a [3]                ├─ P7a [2]
  └─ P7b [3]                └─ P7b    derived: 6 - 2 = 4
*/
abstract sig P7 {}
sig P7a, P7b extends P7 {}

test expect ex7 {
  ex7_sum_plain:      { #P7a = 3 and #P7b = 3 } for 3 P7a, 3 P7b is sat
  ex7_sum_with_min:   { #P7a = 3 and #P7b = 3 } for 3 P7a, 3 P7b for { #P7 >= 1 } is sat

  ex7_diff_plain:     { #P7b = 5 } for 6 P7, 2 P7a is unsat
  ex7_diff_with_min:  { #P7b = 5 } for 6 P7, 2 P7a for { #P7b >= 1 } is unsat
}
