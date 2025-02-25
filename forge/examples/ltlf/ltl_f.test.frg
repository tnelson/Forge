#lang forge

open "ltl_f.frg"

/*
  Rough example-based tests of LTLf semantics.
*/

-- always { eventually { p or q }}
one sig GFPORQ, FPORQ extends Unary {}
one sig PORQ extends Binary {}
one sig P, Q extends Var {}
-- next_state { p }
one sig XP extends Unary {}
-- {p} until {q}
one sig PUQ extends Binary {}
-- not {p or q}
one sig NPORQ extends Unary {}

-- Traces
one sig TraceA, TraceB, TraceC extends Trace {}

pred wellformed_example_gfporq {
    // Set up the formulas
    GFPORQ.uop = Always
    GFPORQ.sub = FPORQ
    FPORQ.uop = Eventually
    FPORQ.sub = PORQ
    PORQ.bop = Or
    PORQ.left = P
    PORQ.right = Q
    XP.uop = Next 
    XP.sub = P
    PUQ.bop = Until
    PUQ.left = P
    PUQ.right = Q
    NPORQ.uop = Not 
    NPORQ.sub = PORQ

    // A: none -> p -> q -> pq  (-8, -7, -6, -5)
    TraceA.endIdx = -5
    TraceA.truths[-8] = none
    TraceA.truths[-7] = P
    TraceA.truths[-6] = Q 
    TraceA.truths[-5] = P + Q
    
    // B: p -> q -> pq (-8, -7, -6)
    TraceB.endIdx = -6
    TraceB.truths[-8] = P
    TraceB.truths[-7] = Q
    TraceB.truths[-6] = P + Q

    // C: none -> p -> none -> q
    TraceC.endIdx = -5
    TraceC.truths[-8] = none
    TraceC.truths[-7] = P
    TraceC.truths[-6] = none
    TraceC.truths[-5] = Q

}

test_gfporq_consistent: assert {
    wellformed_formulas
    wellformed_traces
    semantics
    wellformed_example_gfporq
} is sat for exactly 8 Formula, exactly 3 Trace
    
/* Note this will fail for higher bounds on Formula, 
   since new fmlas might also be satisfied by the traces. */
pred gfporq_semantics {
    Semantics.table[TraceA][-8] = GFPORQ + FPORQ + XP + NPORQ
    Semantics.table[TraceA][-7] = GFPORQ + FPORQ + PORQ + PUQ + P
    Semantics.table[TraceA][-6] = GFPORQ + FPORQ + PORQ + XP + PUQ + Q
    Semantics.table[TraceA][-5] = GFPORQ + FPORQ + PORQ + PUQ + P + Q

    Semantics.table[TraceB][-8] = GFPORQ + FPORQ + PORQ + PUQ + P
    Semantics.table[TraceB][-7] = GFPORQ + FPORQ + PORQ + XP + PUQ + Q
    Semantics.table[TraceB][-6] = GFPORQ + FPORQ + PORQ + PUQ + P + Q

    // C: none -> p -> none -> q
    Semantics.table[TraceC][-8] = GFPORQ + FPORQ + XP + NPORQ
    Semantics.table[TraceC][-7] = GFPORQ + FPORQ + PORQ + P
    Semantics.table[TraceC][-6] = GFPORQ + FPORQ + NPORQ
    Semantics.table[TraceC][-5] = GFPORQ + FPORQ + PORQ + PUQ + Q 
}
test_gfporq_semantics: assert {
    wellformed_formulas
    wellformed_traces
    semantics
    wellformed_example_gfporq 
} 
is sufficient for gfporq_semantics 
for exactly 8 Formula, exactly 3 Trace