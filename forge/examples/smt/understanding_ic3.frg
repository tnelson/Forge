#lang forge 

/*
  The invariant-preservation example from section 1 of 
  "Understanding IC3" by Aaron R. Bradley
  https://theory.stanford.edu/~arbrad/papers/Understanding_IC3.pdf

  Because of the wraparound semantics of Kodkod/Pardinus ints, this is challenging
  to set up. E.g., the direct fix for system 2 fails because Forge can wrap-around 
  6 + 2 = 8 ----> -8 when bitwidth is the default. Hence, we'll use SMT here.
*/

/** Tell Forge to use the cvc5 ToR backend. */
option backend smtlibtor

/** Tell Forge to continue running tests after one fails. */
option test_keep last 

-----------------------------------------------------------
-- System specifications
-----------------------------------------------------------

/** A state contains two integer values: x and y */
sig State { x, y: one Int }

/** System 1 
  x, y := 1, 1
  while *: 
    x, y := x+1, y+x
*/

pred init_1[s: State] { 
    s.x = 1 
    s.y = 1
}

pred delta_1[pre, post: State] { 
    post.x = add[pre.x, 1] 
    post.y = add[pre.y, pre.x]
}

/** System 2 
  x, y := 1, 1
  while *: 
    x, y := x+y, y+x
*/

pred init_2[s: State] { 
    s.x = 1 
    s.y = 1
}

pred delta_2[pre, post: State] { 
    post.x = add[pre.x, pre.y] 
    post.y = add[pre.y, pre.x]
}

/** Goal invariant: y >= 1 */

pred goal[s: State] {
    s.y >= 1
}

-----------------------------------------------------------
-- Proving: Attempt 1
-----------------------------------------------------------

/** Both systems satisfy initiation */
test expect {
    initiation_1: {all s: State | init_1[s] implies goal[s]} for 1 State is theorem
    initiation_2: {all s: State | init_2[s] implies goal[s]} for 1 State is theorem
}

/** *Both* fail consecution. The goal is not inductive.  
    For ease of running the example, these tests are phrased as expectations
    of failure: the negation of the requirement is satisfiable. */
test expect {
    consecution_1: {not {all pre, post: State | 
        (goal[pre] and delta_1[pre, post]) implies goal[post]}} for 2 State is sat
    consecution_2: {not {all pre, post: State | 
        (goal[pre] and delta_2[pre, post]) implies goal[post]}} for 2 State is sat
}

-----------------------------------------------------------
-- Proving: Attempt 2 (focus on system 1)
-----------------------------------------------------------

/** We must identify why induction failed for system 1. 
    The counterexample (which we get if we flip the expectation above and 
    remove the negation) gives a hint: 
    it should be true that `x` remains non-negative. */ 
pred phi_1[s: State] {
    s.x >= 0
}

/** Is phi_1 inductive in system 1? Yes it is. */
test expect {
    phi_1_initiation_sys_1: {all s: State | init_1[s] implies phi_1[s]
      } for 1 State is theorem
    phi_1_consecution_sys_1: {all pre, post: State | 
      (phi_1[pre] and delta_1[pre, post]) implies phi_1[post]
      } for 2 State is theorem
}

/** Trying consecution for system 1 again, relative to phi_1.
    This succeeds! Note that we don't have phi_1 on the right-hand side,
    because we've already proven that it is invariant for system 1.  */
test expect {
    consecution_1_again: {all pre, post: State | 
        (phi_1[pre] and goal[pre] and delta_1[pre, post]) implies 
        (goal[post])} for 2 State is theorem
}

-----------------------------------------------------------
-- Proving: Attempt 2 (focus on system 2)
-----------------------------------------------------------

/** Is phi_1 inductive in system 2? Unfortunately not. So we cannot 
  use it as a helper lemma in checking consecution for the goal. However... */
test expect {
    phi_1_initiation_sys_2: {all s: State | init_2[s] implies phi_1[s]
    } for 1 State is theorem
    phi_1_consecution_sys_2: {not {all pre, post: State | 
      (phi_1[pre] and delta_2[pre, post]) implies phi_1[post]
    }} for 2 State is sat
}

/** Does the same inductive strengthening work on system 2, by combining
    rather than proving a helper lemma? Notice that we _do_ have phi_1 
    on the right-hand side this time, because we cannot assume it globally.
    
    (Yes!) */
test expect {
    consecution_2_again_not_relative: {all pre, post: State | 
        (phi_1[pre] and goal[pre] and delta_2[pre, post]) implies 
        (phi_1[post] and goal[post])} for 2 State is theorem
}
