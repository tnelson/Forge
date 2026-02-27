#lang forge 

option run_sterling off
//option verbose 5 

sig Parent {}
sig Child extends Parent {} 

// Surplus atoms: 3 Parent, 2 Child means Child shares Parent's upper bound.
// Alloy handles this with an existential cardinality formula rather than #Child <= 2.
// Forge uses cardinality constraints (or the partition optimization to avoid them).
TEST_check_bounds_surplus: assert {} is sat for // forge_error for 
    3 Parent, 2 Child
    , 2 Int

// TODO: the assert form doesn't support the regex string for `is forge_error`

// This is unsatisfiable, which agrees with Alloy's behavior. 
TEST_check_bounds_shortfall: assert {#Child = 3} is unsat for 
   2 Parent, 3 Child



/*
  Top-down allocation (parent first) gives all children the parent's full upper bound,
  requiring cardinality constraints (#Child <= 2) to enforce scopes. Exact bounds skip
  this since they get dedicated atoms.

  Bottom-up allocation (children first) would avoid cardinality constraints by giving
  each child its own atoms, then unioning them for the parent. The partition optimization
  in send-to-solver.rkt takes this approach when feasible (clean shared pool, children
  fit within surplus). Partial instances: a parent can be inst-bound (with user-chosen
  or generated names) while children are scope-based; surplus atoms are then drawn from
  the parent's inst-provided pool. Partition is skipped if any relation inst (field
  binding or piecewise bound) references a surplus atom by name
  (see bounds-partition-inst.frg, Sections J-b, P). Binding a child sig via inst while
  the parent is scope-based errors. Subset sigs use a different bounds path.
*/
