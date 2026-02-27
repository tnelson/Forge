#lang forge

option run_sterling off
option verbose 0

// ======================================================================
// SAT/UNSAT Equivalence Tests for Atom Partition Optimization
//
// For each scenario, we test with both exact and non-exact bounds.
// Key invariant: if exact → SAT, then non-exact → must also be SAT.
// Violation of this invariant means the partition wrongly excluded
// valid instances.
// ======================================================================

// ------------------------------------------------------------------
// Scenario 1: Flat hierarchy with tight field constraints
// "Every key is owned by exactly one agent"
// ------------------------------------------------------------------

abstract sig Key1 {}
sig PK1 extends Key1 {}
sig SK1 extends Key1 {}

sig Agent1 { owns1: set Key1 }

pred scenario1 {
    all k: Key1 | one owns1.k
    all a: Agent1 | some a.owns1
    #PK1 = #SK1
}

test expect Equiv_Scenario1 {
    s1_exact: { scenario1 }
        for exactly 6 Key1, exactly 3 PK1, exactly 3 SK1, exactly 2 Agent1 is sat

    s1_nonexact: { scenario1 }
        for 6 Key1, 3 PK1, 3 SK1, 2 Agent1 is sat

    -- Tighter: children sum = parent exactly
    s1_exact_tight: { scenario1 }
        for exactly 4 Key1, exactly 2 PK1, exactly 2 SK1, exactly 2 Agent1 is sat

    s1_nonexact_tight: { scenario1 }
        for 4 Key1, 2 PK1, 2 SK1, 2 Agent1 is sat
}

// ------------------------------------------------------------------
// Scenario 2: Cross-sibling fields (pair: Pub -> Priv)
// Forces the solver to find atoms across partition boundaries
// ------------------------------------------------------------------

abstract sig Key2 {}
abstract sig AKey2 extends Key2 {}
sig Priv2 extends AKey2 {}
sig Pub2 extends AKey2 {
    pair2: one Priv2
}
sig Sym2 extends Key2 {}

pred scenario2 {
    -- Injective pairing
    all p1, p2: Pub2 | p1 != p2 implies p1.pair2 != p2.pair2
    -- All privates are paired
    all pr: Priv2 | some pair2.pr
    #Priv2 = #Pub2
    some Sym2
}

test expect Equiv_Scenario2 {
    s2_exact: { scenario2 }
        for exactly 7 Key2, exactly 6 AKey2, exactly 3 Priv2, exactly 3 Pub2,
            exactly 1 Sym2 is sat

    s2_nonexact: { scenario2 }
        for 7 Key2, 6 AKey2, 3 Priv2, 3 Pub2, 1 Sym2 is sat

    -- Perfect fit at AKey2 level (3+3=6), 1 Sym for scenario2's `some Sym2`
    s2_exact_perfect: { scenario2 }
        for exactly 7 Key2, exactly 6 AKey2, exactly 3 Priv2, exactly 3 Pub2,
            exactly 1 Sym2 is sat

    s2_nonexact_perfect: { scenario2 }
        for 7 Key2, 6 AKey2, 3 Priv2, 3 Pub2, 1 Sym2 is sat
}

// ------------------------------------------------------------------
// Scenario 3: 3-level hierarchy with high utilization
// Forces solver to use many atoms from each child's partition subset
// ------------------------------------------------------------------

abstract sig Root3 {}
abstract sig Mid3A extends Root3 {}
sig Leaf3A extends Mid3A {}
sig Leaf3B extends Mid3A {}
sig Mid3B extends Root3 {}

pred scenario3 {
    -- Force high utilization: at least 2 of each leaf
    #Leaf3A >= 2
    #Leaf3B >= 2
    #Mid3B >= 2
    -- Total must fill parent
    Root3 = Mid3A + Mid3B
}

test expect Equiv_Scenario3 {
    s3_exact: { scenario3 }
        for exactly 6 Root3, exactly 4 Mid3A, exactly 2 Leaf3A, exactly 2 Leaf3B,
            exactly 2 Mid3B is sat

    s3_nonexact: { scenario3 }
        for 6 Root3, 4 Mid3A, 2 Leaf3A, 2 Leaf3B, 2 Mid3B is sat
}

// ------------------------------------------------------------------
// Scenario 4: one sig + normal siblings + field constraints
// ------------------------------------------------------------------

abstract sig Base4 {}
one sig Special4 extends Base4 {}
sig Normal4A extends Base4 { link4: lone Base4 }
sig Normal4B extends Base4 {}

pred scenario4 {
    -- Every Normal4A links to something
    all n: Normal4A | some n.link4
    -- At least one link crosses to Normal4B
    some n: Normal4A | n.link4 in Normal4B
    -- Special4 is linked to
    some link4.Special4
    some Normal4B
}

test expect Equiv_Scenario4 {
    s4_exact: { scenario4 }
        for exactly 5 Base4, exactly 2 Normal4A, exactly 2 Normal4B is sat

    s4_nonexact: { scenario4 }
        for 5 Base4, 2 Normal4A, 2 Normal4B is sat
}

// ------------------------------------------------------------------
// Scenario 5: Near-boundary — children sum = parent - 1 (one slack)
// ------------------------------------------------------------------

abstract sig Slot5 {}
sig TypeA5 extends Slot5 {}
sig TypeB5 extends Slot5 {}

pred scenario5 {
    #TypeA5 >= 2
    #TypeB5 >= 2
    Slot5 = TypeA5 + TypeB5
}

test expect Equiv_Scenario5 {
    -- Slack=1: exact 2+2=4 != 5, so exact is unsat.
    -- Non-exact: solver can use 4 of 5 Slot5 atoms, so sat.
    s5_exact_slack: { scenario5 }
        for exactly 5 Slot5, exactly 2 TypeA5, exactly 2 TypeB5 is unsat

    s5_nonexact_slack: { scenario5 }
        for 5 Slot5, 2 TypeA5, 2 TypeB5 is sat

    -- Slack=0: 3+2=5, parent=5
    s5_exact_tight: { scenario5 }
        for exactly 5 Slot5, exactly 3 TypeA5, exactly 2 TypeB5 is sat

    s5_nonexact_tight: { scenario5 }
        for 5 Slot5, 3 TypeA5, 2 TypeB5 is sat
}

// ------------------------------------------------------------------
// Scenario 6: UNSAT models — partition must not cause false SAT
// ------------------------------------------------------------------

abstract sig U6 {}
sig UA6 extends U6 {}
sig UB6 extends U6 {}

test expect Equiv_Scenario6 {
    -- Disjointness is always enforced
    s6_exact_disj: { some (UA6 & UB6) }
        for exactly 4 U6, exactly 2 UA6, exactly 2 UB6 is unsat

    s6_nonexact_disj: { some (UA6 & UB6) }
        for 4 U6, 2 UA6, 2 UB6 is unsat

    -- Over-requesting: ask for more children than parent allows
    s6_exact_over: { #UA6 = 3 and #UB6 = 3 }
        for exactly 4 U6, exactly 3 UA6, exactly 3 UB6 is unsat

    s6_nonexact_over: { #UA6 = 3 and #UB6 = 3 }
        for 4 U6, 3 UA6, 3 UB6 is unsat
}

// ------------------------------------------------------------------
// Scenario 7: lone sig + abstract child + grandchildren
// Complex hierarchy shape
// ------------------------------------------------------------------

sig Top7 {}
abstract sig AbsChild7 extends Top7 {}
sig Grand7A extends AbsChild7 {}
sig Grand7B extends AbsChild7 {}
lone sig LoneChild7 extends Top7 {}

pred scenario7 {
    some Grand7A
    some Grand7B
    AbsChild7 = Grand7A + Grand7B
}

test expect Equiv_Scenario7 {
    s7_exact: { scenario7 }
        for exactly 5 Top7, exactly 4 AbsChild7,
            exactly 2 Grand7A, exactly 2 Grand7B is sat

    s7_nonexact: { scenario7 }
        for 5 Top7, 4 AbsChild7, 2 Grand7A, 2 Grand7B is sat
}

// ------------------------------------------------------------------
// Scenario 8: Cardinality constraint across siblings
// #Sib1 = #Sib2 forces equal population from different partition slices
// ------------------------------------------------------------------

abstract sig Parent8 {}
sig Sib8A extends Parent8 {}
sig Sib8B extends Parent8 {}
sig Sib8C extends Parent8 {}

pred scenario8 {
    #Sib8A = #Sib8B
    #Sib8B = #Sib8C
    #Sib8A >= 1
}

test expect Equiv_Scenario8 {
    s8_exact: { scenario8 }
        for exactly 6 Parent8, exactly 2 Sib8A, exactly 2 Sib8B, exactly 2 Sib8C is sat

    s8_nonexact: { scenario8 }
        for 6 Parent8, 2 Sib8A, 2 Sib8B, 2 Sib8C is sat

    -- Tight: 1+1+1=3 with parent=3
    s8_nonexact_tight: { scenario8 }
        for 3 Parent8, 1 Sib8A, 1 Sib8B, 1 Sib8C is sat
}

// ------------------------------------------------------------------
// Scenario 9: 3-level — partition fires at root, skips at mid
// Tests that leaves still get correct instances when mid-level
// over-commits independently of the root-level partition.
// ------------------------------------------------------------------

abstract sig Root9 {}
abstract sig Mid9 extends Root9 {}
sig Leaf9A extends Mid9 {}
sig Leaf9B extends Mid9 {}
sig Other9 extends Root9 {}

pred scenario9 {
    some Leaf9A
    some Leaf9B
    some Other9
    Root9 = Mid9 + Other9
}

test expect Equiv_Scenario9 {
    s9_exact: { scenario9 }
        for exactly 6 Root9, exactly 3 Mid9, exactly 2 Leaf9A, exactly 1 Leaf9B,
            exactly 3 Other9 is sat

    s9_nonexact: { scenario9 }
        for 6 Root9, 3 Mid9, 2 Leaf9A, 2 Leaf9B, 3 Other9 is sat

    -- Tighter leaves: both at 1
    s9_nonexact_tight: { scenario9 }
        for 6 Root9, 3 Mid9, 1 Leaf9A, 1 Leaf9B, 3 Other9 is sat
}

// ------------------------------------------------------------------
// Scenario 10: Kitchen sink — one + lone + abstract-with-children + normal
// Tests equivalence across all four sibling types.
// ------------------------------------------------------------------

abstract sig Parent10 {}
one sig One10 extends Parent10 {}
lone sig Lone10 extends Parent10 {}
abstract sig Abs10 extends Parent10 {}
sig Grand10A extends Abs10 {}
sig Grand10B extends Abs10 {}
sig Norm10 extends Parent10 {}

pred scenario10 {
    some Lone10
    some Grand10A
    some Grand10B
    some Norm10
    Abs10 = Grand10A + Grand10B
}

test expect Equiv_Scenario10 {
    s10_exact: { scenario10 }
        for exactly 8 Parent10, exactly 4 Abs10, exactly 2 Grand10A,
            exactly 2 Grand10B, exactly 2 Norm10 is sat

    s10_nonexact: { scenario10 }
        for 8 Parent10, 4 Abs10, 2 Grand10A, 2 Grand10B, 2 Norm10 is sat
}
