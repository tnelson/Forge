#lang forge
/*
  Modeling finite-trace LTL in Forge
  TN January 2023

  LTLf is similar to standard LTL, except that it is interpreted over finite traces. 
  In some ways, this makes it similar to the finite-trace idiom we used to model 
  executions before moving to Relational Forge, but there are key differences. 
  As a result, it might be especially useful to model it and make sure we understand 
  it! (Please remember: this is a different logic than what Temporal Forge uses.)

  I'm modeling this a bit differently from the boolean-logic model to show a few 
  techniques that might be useful. 
  
  We want to represent multiple traces in the same instance. So we need to 
  separate "state" from "state index", either by using Int or by making 
  a new Index type that must be ordered. I decided to use Ints, and to avoid 
  needing an optimizer instance I'm just declaring traces to start at min[Int].
*/

/** Disable integer overflow, since we use ints as state indexes */
option no_overflow true

-------------------------------------------------------
-- Formulas of LTLf 
--   leaving out R(elease) and X_w, since they can be desugared
--   via X, Not, and U. Similarly, leaving out implies and iff.
-------------------------------------------------------

/** Enum tags for operators */
abstract sig UOP, BOP {}
one sig Not, Next, Eventually, Always extends UOP {}
one sig And, Or, Until extends BOP {}

/** Formulas are either atomic propositions, unary operator applications, or binary 
    operator applications. */
abstract sig Formula {}
/** We called this "Var" in the boolean-logic model. */
sig Var extends Formula {}
sig Unary extends Formula {
    uop: one UOP,
    sub: one Formula
}
sig Binary extends Formula {
    bop: one BOP,
    left: one Formula,
    right: one Formula
}

/** Notice how using enum tags simplifies this helper function. */
fun subformulas[fmla: Formula]: set Formula {
    fmla.^(sub + left + right)
}
pred wellformed_formulas {
    all f: Formula | f not in subformulas[f]
}

-------------------------------------------------------
-- Semantics: Finite traces
-------------------------------------------------------

/** Finite, possibly empty sequences of states. In contrast to the 
    boolean-logic model, we're using integers as time-indexes. But since 
    these traces are finite, we need to keep track of the ending index. 
    
    Ideally, we'd declare traces start at idx=0, and use an optimizer instance 
    to deal with this. 
    */
sig Trace {    
    /** The last populated index of the trace. We assume traces are not empty. */
    endIdx: one Int,
    /** The atomic truths at each state: which variables are set to true? */
    truths: set Int -> Var
}

/** Helper that returns the set of integer indexes that are valid for this trace. */
fun states[t: Trace]: set Int {
    -- every pre-state and every post-state in the trace
    {i: Int | i <= t.endIdx}
}

/** Helper that returns the set of integer indexes that are both valid and >= s. */
fun laterOrNow[t: Trace, s: Int]: set Int {
    {i: Int | i <= t.endIdx and i >= s}
}

pred wellformed_traces {
    -- There are no truths outside the valid indexes for this trace. 
    all t: Trace | t.truths.Var in states[t]
    -- There are no lifted truths outside the valid indexes for this trace.
    all t: Trace | (Semantics.table[t]).Var in states[t]
}

one sig Semantics {
    /** This could be a field of Trace as well, but I pulled it out while I was 
    working on fixing an issue and kept it this way. This is a "lifting" of the 
    `truths` relation to arbitrary formulas.  */
    table: set Trace -> Int -> Formula
}

/** Same trick as in boolean logic. 
    This trick won't always work, but it does for tree-shaped data.
    
    Using definitions from https://cs.brown.edu/~tbn/publications/ltlf-misconceptions.pdf
*/
pred semantics {
    all t: Trace, s: Int, f: Formula | t->s->f in Semantics.table iff { 
        s in states[t] and {
        -- Var case: if the state at index <s> sets this variable to true
        f in Var and f in t.truths[s]
        or
        -- Not case
        f.uop = Not and {
            t->s->(f.sub) not in Semantics.table           
        }
        or
        -- Eventually case
        f.uop = Eventually and {            
            some s2 : laterOrNow[t, s] | t->s2->(f.sub) in Semantics.table            
        }
        or
        -- Always case
        f.uop = Always and {
            all s2 : laterOrNow[t, s] | t->s2->(f.sub) in Semantics.table
        }
        or
        -- Next case
        f.uop = Next and {
            t->add[s,1]->(f.sub) in Semantics.table
        }
        or
        -- And case
        f.bop = And and {
            t->s->(f.left) in Semantics.table and
            t->s->(f.right) in Semantics.table
        }
        or
        -- Or case
        f.bop = Or and {
            t->s->(f.left) in Semantics.table or
            t->s->(f.right) in Semantics.table
        }
        or
        -- Until case (*easier* to express without lasso traces!)
        f.bop = Until and {
            some s2: laterOrNow[t, s] | {
                t->s2->(f.right) in Semantics.table
                -- Note that the LHS obligation doesn't hold in the moment the RHS becomes true. 
                all smid: Int | (smid >= s and smid < s2) implies 
                                  t->smid->(f.left) in Semantics.table

            }
        }
    }}

}

exampleForVisualizing: run {
    wellformed_formulas
    wellformed_traces
    semantics
} for exactly 5 Formula, exactly 3 Var, exactly 2 Trace
