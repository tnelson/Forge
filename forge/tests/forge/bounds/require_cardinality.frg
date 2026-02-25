#lang forge 

option run_sterling off
//option verbose 5 

sig Parent {}
sig Child extends Parent {} 

// Error. Hypothesis: we don't need cardinality here, after all. There's a surplus. 
// But this is satisfiable in Alloy. It must be ignoring the Int parameter? Or else 
// it's doing something smarter and the debug output isn't reflecting that.
/*
   Sig this/Parent scope <= 3
      Sig this/Child scope <= 2
      Sig this/Parent in [[Parent$0], [Parent$1], [Parent$2]]
      Sig this/Child in [[Parent$0], [Parent$1], [Parent$2]] with size<=2

    Modifying A4Solution.java:solve() with rep.debug(...);

Bounds: 
    this/Child: [[], [[Parent$0], [Parent$1], [Parent$2]]] 
    this/Parent_remainder: [[], [[Parent$0], [Parent$1], [Parent$2]]] 

Formulas:
    [(no this/Child or (some [v1: one this/Child, v0: one this/Child] | ((v1 + v0) = this/Child)))]
   
    *** So it _is_ doing something smarter than cardinality. 
    *** But it also isn't limiting the upper bound. 

*/
TEST_check_bounds_surplus: assert {} is sat for // forge_error for 
    3 Parent, 2 Child
    , 2 Int

// TODO: the assert form doesn't support the regex string for `is forge_error`

// This is unsatisfiable, which agrees with Alloy's behavior. 
TEST_check_bounds_shortfall: assert {#Child = 3} is unsat for 
   2 Parent, 3 Child



/* 
  If we start assigning atoms at the parent, then we get {P0, P1, P2} and we have no idea
  which of them should be assigned to the child. So we add a cardinality constraint:
    #Child <= 2. 
  This isn't needed if there's an `exact` bound on Child, since those get enumerated atoms. 

  Alloy seems to do the same (at least, its debug output reports that it does):
    Sig this/Parent in [[Parent$0], [Parent$1], [Parent$2]]
    Sig this/Child in [[Parent$0], [Parent$1], [Parent$2]] with size<=2

  Suppose we started at the bottom instead. 
    Child: {} : {(Atom0), (Atom1)}
  Now those 2 atom names are used, and in the upper bound of the parent, which needs to add 1 more:
    Parent:  {} : {(Atom0), (Atom1), (Atom2)}
  
  The atom names are even less descriptive. But let's explore this idea anyway. 
    (1) DFS the sig hierarchy. 
    (2) At a leaf, allocate upper bound (even if not exact)
    (3) when returning, take union of child upper bounds, then add more if needed. 
    (4) pass over the tree again, renaming atoms (one, exactly, ... or top level)
  
  Q1: Is this sound? Alloy doesn't do this, apparently, even though it builds bottom up (I think?)
  Q2: Does this cause trouble with partial instances?

    'lone' sigs?
    subset sigs?

  

*/
    