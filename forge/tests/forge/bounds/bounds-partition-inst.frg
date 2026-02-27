#lang forge

option run_sterling off
option verbose 0

// ======================================================================
// Tests for mixing inst (relational bounds) with numeric scopes.
// Verifies that the partition optimization correctly handles inst bounds.
// ======================================================================

// --------------------------------------------------------------------
// Section J-a: Independent hierarchies
//   inst-based bounds on one root, scope-based on another root.
//   Verifies they don't interfere.
// --------------------------------------------------------------------

sig Alpha {}
sig Beta extends Alpha {}
sig Gamma extends Alpha {}

sig Other {}

-- Other is fully bound by inst; Alpha hierarchy uses scope only
inst instOther {
    Other = `Other0 + `Other1 + `Other2
}

test expect Ja_IndependentHierarchies {
    -- Basic: scope-based hierarchy + inst-based hierarchy coexist
    ja_basic: { some Alpha and some Other }
        for 4 Alpha, 2 Beta, 2 Gamma for instOther is sat

    -- Children at full scope while inst hierarchy is fixed
    ja_full_children: { #Beta = 2 and #Gamma = 2 }
        for 4 Alpha, 2 Beta, 2 Gamma for instOther is sat

    -- Abstract-like behavior: children consume parent
    ja_all_children: { Alpha = Beta + Gamma }
        for 4 Alpha, 2 Beta, 2 Gamma for instOther is sat

    -- Inst hierarchy cardinality is fixed
    ja_other_fixed: { #Other != 3 }
        for 4 Alpha, 2 Beta, 2 Gamma for instOther is unsat

    -- Over-committed children (should still be sat with reduced children)
    ja_overcommit: { some Beta and some Gamma }
        for 3 Alpha, 2 Beta, 2 Gamma for instOther is sat
}

// --------------------------------------------------------------------
// Section J-b: Relation-only inst with potential surplus-atom overlap
//   An inst binds a FIELD but not the sigs. For non-exact, non-one
//   children, surplus atoms are named after the root sig (e.g., Parent0).
//   So `link = `Parent0 -> `Parent0` references surplus atoms, which
//   prevents partition to avoid conflicts.
// --------------------------------------------------------------------

sig Parent {}
sig ChildA extends Parent {
    link: lone Parent
}
sig ChildB extends Parent {}

// This inst binds only the `link` relation, NOT the sigs.
// `Parent0` is a surplus atom (named after the root).
inst instRelOnly {
    link = `Parent0 -> `Parent0
}

test expect Jb_RelationOnlyInst {
    -- Parent0 is a surplus atom referenced by inst, so partition is skipped.
    -- All parent atoms are in ChildA.upper; ChildA can contain Parent0.
    jb_basic: { some link }
        for 4 Parent, 2 ChildA, 2 ChildB for instRelOnly is sat

    -- link has a bound, children should still be usable
    jb_children: { #ChildA >= 1 and #ChildB >= 1 }
        for 4 Parent, 2 ChildA, 2 ChildB for instRelOnly is sat
}

// --------------------------------------------------------------------
// Section J-c: Relation inst on unrelated hierarchy (no false positives)
//   inst binds a field using atoms from hierarchy X.
//   hierarchy Y (scope-based, with children) should be unaffected.
// --------------------------------------------------------------------

sig Src {}
sig Dst {
    edge: set Src
}

sig Animal {}
sig Dog extends Animal {}
sig Cat extends Animal {}

// inst fully binds Src/Dst/edge — completely unrelated to Animal hierarchy
inst instEdge {
    Src = `Src0 + `Src1
    Dst = `Dst0
    edge = `Dst0 -> `Src0 + `Dst0 -> `Src1
}

test expect Jc_UnrelatedHierarchies {
    -- Animal hierarchy is scope-based and should work normally
    jc_basic: { some Dog and some Cat }
        for 4 Animal, 2 Dog, 2 Cat for instEdge is sat

    -- Full scope for children
    jc_full: { #Dog = 2 and #Cat = 2 }
        for 4 Animal, 2 Dog, 2 Cat for instEdge is sat

    -- Inst hierarchy is fixed
    jc_edge_fixed: { #edge != 2 }
        for 4 Animal, 2 Dog, 2 Cat for instEdge is unsat

    -- Over-committed: children sum > parent. Can't have both at max.
    jc_overcommit_unsat: { #Dog = 2 and #Cat = 2 }
        for 3 Animal, 2 Dog, 2 Cat for instEdge is unsat

    -- But can have some of each (1+1 <= 3)
    jc_overcommit_partial: { some Dog and some Cat }
        for 3 Animal, 2 Dog, 2 Cat for instEdge is sat
}

// --------------------------------------------------------------------
// Section J-d: Relation inst referencing lower-bound atoms
//   Lower-bound atoms come from `one sig` or `exactly N sig`.
//   These are named after the CHILD sig (e.g., Car0, Truck0),
//   not the root. They live in the child's lower bound, NOT in the
//   root's surplus atoms. Partition should not affect them.
//
//   Test 1: `one sig` atom (Car0) used in a cross-hierarchy relation.
//   Test 2: `exactly N` atoms (Truck0...) used similarly.
// --------------------------------------------------------------------

sig Vehicle {}
one sig Car extends Vehicle {}
sig Truck extends Vehicle {}

// A separate hierarchy that references Vehicle atoms via a field
sig Connector {
    target: lone Vehicle
}

// Uses Car0, which is a lower-bound atom from `one sig Car`.
// Car0 is always in Vehicle.upper regardless of partition.
inst instTargetOneSig {
    target = `Connector0 -> `Car0
}

test expect Jd_LowerBoundAtoms_OneSig {
    -- Car0 is always in Vehicle.upper (it's in Vehicle.lower, propagated from Car)
    jd_one_basic: { some target }
        for 4 Vehicle, 3 Truck, 2 Connector for instTargetOneSig is sat

    -- Truck can reach full scope alongside the one-sig Car
    jd_one_truck: { #Truck = 3 }
        for 4 Vehicle, 3 Truck, 2 Connector for instTargetOneSig is sat

    -- Car is exactly one
    jd_one_car: { #Car != 1 }
        for 4 Vehicle, 3 Truck, 2 Connector for instTargetOneSig is unsat
}

// Uses Truck0, which is a lower-bound atom from `exactly 3 Truck`.
// With exact scope, fill-lower-by-scope generates Truck0..Truck2.
// These are in Truck.lower and thus Vehicle.lower — not surplus atoms.
inst instTargetExact {
    target = `Connector0 -> `Truck0
}

test expect Jd_LowerBoundAtoms_Exact {
    -- Truck0 exists because `exactly 3 Truck` generates it in fill-lower-by-scope
    jd_exact_basic: { some target }
        for 4 Vehicle, exactly 3 Truck, 2 Connector for instTargetExact is sat

    -- Truck fills its exact scope
    jd_exact_full: { #Truck = 3 }
        for 4 Vehicle, exactly 3 Truck, 2 Connector for instTargetExact is sat
}

// Negative test: Truck0 does NOT exist under non-exact scope (atoms are named Vehicle0, etc.)
// This should be an error — Truck0 is not in any sig's bounds.
// We can't easily test for errors in test-expect, so this is documented rather than tested.
// (Under non-exact `3 Truck`, the atoms in Truck.upper are Vehicle0..Vehicle3, not Truck0.)

// --------------------------------------------------------------------
// Section J-b2: Relation-only inst with FIELD on child sig
//   Like J-b but the field is declared on a child sig, making
//   the child's upper bound the relevant check, not the parent's.
//   This is the most dangerous scenario for partition.
// --------------------------------------------------------------------

sig Top {}
sig MidA extends Top {
    payload: lone Top
}
sig MidB extends Top {}

// Binds `payload` using Top0 — a surplus atom from the root.
// payload is declared on MidA, so Top0 must be in MidA.upper.
// Since inst references a surplus atom, partition is skipped,
// ensuring Top0 remains in MidA.upper.
inst instPayload {
    payload = `Top0 -> `Top0
}

test expect Jb2_FieldOnChild {
    -- Top0 must be in MidA.upper for this to work.
    -- Since Top0 is a surplus atom referenced by inst, partition is skipped.
    jb2_basic: { some payload }
        for 4 Top, 2 MidA, 2 MidB for instPayload is sat

    -- MidA and MidB can both be populated
    jb2_siblings: { #MidA >= 1 and #MidB >= 1 }
        for 4 Top, 2 MidA, 2 MidB for instPayload is sat

    -- Children at full scope
    jb2_full: { #MidA = 2 and #MidB = 2 }
        for 4 Top, 2 MidA, 2 MidB for instPayload is sat
}

// --------------------------------------------------------------------
// Section N: Piecewise bounds on fields of scope-based sigs
//   FINDING: This scenario is IMPOSSIBLE. Piecewise bounds require sig
//   bindings on the child (to name atoms). Sig bindings on any child
//   force fill-upper-no-bound (line 557) to error with "Please specify
//   an upper bound for ancestors." So the scope-based path and piecewise
//   path are mutually exclusive per hierarchy — they can never interact.
//   This is actually good: partition only runs on the scope-based path,
//   so piecewise bounds are unreachable from the partition code.
// --------------------------------------------------------------------

// FINDING: Binding a SIG in an inst (e.g., `Branch = ...`) while its parent is
// scope-based causes error: "Please specify an upper bound for ancestors of Branch."
// BUT: PIECEWISE bounds on a RELATION (e.g., `Parent0.link = ...`) do NOT bind
// the sig. They go into Bound-piecewise, not Bound-pbindings. So the sig stays
// scope-based and no error occurs. This IS a live interaction.

sig PwParent {}
sig PwChildA extends PwParent {
    pwlink: lone PwParent
}
sig PwChildB extends PwParent {}

// Piecewise bound: binds relation `pwlink` using atom `PwParent0`.
// PwParent0 matches a name generated by the scope-based path.
// No sig is bound — all sigs go through the scope path.
inst instPiecewise {
    `PwParent0.pwlink = `PwParent0
}

test expect N_PiecewiseBounds {
    -- PwParent0 is a surplus atom referenced by piecewise inst,
    -- so partition is skipped, keeping PwParent0 in PwChildA.upper.
    n_pw_basic: { some pwlink }
        for 4 PwParent, 2 PwChildA, 2 PwChildB for instPiecewise is sat

    -- Piecewise forces PwParent0 into PwChildA (it's the domain of pwlink).
    -- Children should still be usable.
    n_pw_siblings: { #PwChildA >= 1 and #PwChildB >= 1 }
        for 4 PwParent, 2 PwChildA, 2 PwChildB for instPiecewise is sat

    -- Full scope for children
    n_pw_full: { #PwChildA = 2 and #PwChildB = 2 }
        for 4 PwParent, 2 PwChildA, 2 PwChildB for instPiecewise is sat
}

// Binding a child sig while parent is scope-based should error.
inst instBadSigBind { PwChildA = `A0 + `A1 }

test expect N_ErrorCases {
    bad_child_bind: { some PwChildA }
        for 4 PwParent, 2 PwChildA, 2 PwChildB for instBadSigBind
        is forge_error "upper bound"
}

// --------------------------------------------------------------------
// Section P: Custom atom names (user-named parent, scope-based children)
//   Users can bind a parent sig with custom names (e.g., `Alice) while
//   children remain scope-based. The children's upper bounds draw from
//   the parent's custom-named atom pool instead of generated names.
// --------------------------------------------------------------------

sig CParent {}
sig CChildA extends CParent {
    clink: lone CParent
}
sig CChildB extends CParent {}

inst instCustomParent {
    CParent = `Alice + `Bob + `Carol + `Dave
}

inst instCustomParentAndField {
    CParent = `Alice + `Bob + `Carol + `Dave
    clink = `Alice -> `Bob
}

test expect P_CustomAtomNames {
    -- Children draw from custom-named parent atoms
    p_basic: { some CChildA and some CChildB }
        for 4 CParent, 2 CChildA, 2 CChildB for instCustomParent is sat

    p_full: { #CChildA = 2 and #CChildB = 2 }
        for 4 CParent, 2 CChildA, 2 CChildB for instCustomParent is sat

    p_disjoint: { some (CChildA & CChildB) }
        for 4 CParent, 2 CChildA, 2 CChildB for instCustomParent is unsat

    p_parent_fixed: { not (#CParent = 4) }
        for 4 CParent, 2 CChildA, 2 CChildB for instCustomParent is unsat

    -- Field inst with custom parent names: partition skipped (inst atoms
    -- overlap with surplus), so Alice stays available to both children.
    p_field: { some clink and some CChildB }
        for 4 CParent, 2 CChildA, 2 CChildB for instCustomParentAndField is sat

    p_field_full: { #CChildA = 2 and #CChildB = 2 and some clink }
        for 4 CParent, 2 CChildA, 2 CChildB for instCustomParentAndField is sat

    -- Alice is in clink's domain, so she must be in CChildA
    p_field_forced: { not (`Alice in CChildA) }
        for 4 CParent, 2 CChildA, 2 CChildB for instCustomParentAndField is unsat
}

// --------------------------------------------------------------------
// Section Q: Multi-level hierarchy with inst at various levels
// --------------------------------------------------------------------

sig MRoot {}
sig MMid extends MRoot {}
sig MLeafA extends MMid {}
sig MLeafB extends MMid {}
sig MOther extends MRoot {}

// Root inst-bound with custom names, all descendants scope-based
inst instMRootCustom {
    MRoot = `R1 + `R2 + `R3 + `R4 + `R5 + `R6
}

test expect Q_MultiLevelInst {
    -- Custom root atoms flow through multi-level scope-based descendants
    q_basic: { some MLeafA and some MLeafB and some MOther }
        for 6 MRoot, 4 MMid, 2 MLeafA, 2 MLeafB, 2 MOther
        for instMRootCustom is sat

    q_full: { #MLeafA = 2 and #MLeafB = 2 and #MOther = 2 }
        for 6 MRoot, 4 MMid, 2 MLeafA, 2 MLeafB, 2 MOther
        for instMRootCustom is sat

    q_mid_other_disjoint: { some (MMid & MOther) }
        for 6 MRoot, 4 MMid, 2 MLeafA, 2 MLeafB, 2 MOther
        for instMRootCustom is unsat

    q_leaves_disjoint: { some (MLeafA & MLeafB) }
        for 6 MRoot, 4 MMid, 2 MLeafA, 2 MLeafB, 2 MOther
        for instMRootCustom is unsat

    -- Intermediate inst binding with scope-based root should error
    q_intermediate_errors: { some MMid }
        for 5 MRoot, 3 MMid, 2 MLeafA, 1 MLeafB
        for { MMid = `M0 + `M1 + `M2 }
        is forge_error "upper bound"
}

// --------------------------------------------------------------------
// Baseline hierarchy tests: behavior the partition targets
// --------------------------------------------------------------------

sig Key {}
sig AsymKey extends Key {}
sig PrivKey extends AsymKey {}
sig PubKey extends AsymKey {
    pair: lone PrivKey
}
sig SymKey extends Key {}

test expect Baseline_CryptoHierarchy {
    -- Perfect fit: children sum = parent
    bl_perfect: { #PrivKey = 3 and #PubKey = 3 and #SymKey = 3 }
        for 9 Key, 6 AsymKey, 3 PrivKey, 3 PubKey, 3 SymKey is sat

    -- Under-committed: children sum < parent
    bl_under: { some PrivKey and some PubKey and some SymKey }
        for 9 Key, 4 AsymKey, 2 PrivKey, 2 PubKey, 3 SymKey is sat

    -- Over-committed: children sum > parent (at AsymKey level: 3+3=6 > 5)
    bl_over: { some PrivKey and some PubKey }
        for 5 Key, 5 AsymKey, 3 PrivKey, 3 PubKey is sat

    -- Cross-sibling field: pair maps PubKey -> PrivKey (siblings)
    bl_cross_field: { (no SymKey) and (all pk: PubKey | some pk.pair) }
        for 6 Key, 6 AsymKey, 3 PrivKey, 3 PubKey, 0 SymKey is sat

    -- Disjointness: siblings never overlap
    bl_disjoint: { some (PrivKey & PubKey) }
        for 6 Key, 6 AsymKey, 3 PrivKey, 3 PubKey is unsat

    bl_disjoint2: { some (AsymKey & SymKey) }
        for 9 Key, 6 AsymKey, 3 SymKey is unsat

    -- Multi-level: 3-level hierarchy (smaller scopes to fit default bitwidth)
    bl_multi: { #Key = 6 and #AsymKey = 4 and #PrivKey = 2 and #PubKey = 2 and #SymKey = 2 }
        for 6 Key, 4 AsymKey, 2 PrivKey, 2 PubKey, 2 SymKey is sat
}
