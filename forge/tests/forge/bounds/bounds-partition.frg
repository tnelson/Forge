#lang forge

option run_sterling off
option verbose 0

// ======================================================================
// Regression tests for the atom partition optimization in fill-upper-no-bound.
// ======================================================================

// ---- Section A: Perfect partition ----
// Abstract parent, children scopes sum exactly to parent.
// Partition assigns each child a disjoint subset of the parent's surplus atoms.

abstract sig AParent {}
sig AChild1 extends AParent {}
sig AChild2 extends AParent {}

test expect A_PerfectPartition {
    -- Basic: both children populated
    a_basic: { some AChild1 and some AChild2 }
        for 6 AParent, 3 AChild1, 3 AChild2 is sat

    -- Children at full scope (3+3 = 6 = parent)
    a_full: { #AChild1 = 3 and #AChild2 = 3 }
        for 6 AParent, 3 AChild1, 3 AChild2 is sat

    -- Abstract parent = union of children
    a_abstract: { AParent = AChild1 + AChild2 }
        for 6 AParent, 3 AChild1, 3 AChild2 is sat

    -- Disjointness: siblings never overlap
    a_disjoint: { some (AChild1 & AChild2) }
        for 6 AParent, 3 AChild1, 3 AChild2 is unsat
}

// ---- Section B: Under-partition ----
// Non-abstract parent with slack atoms (parent can have its own atoms).
// Children sum < parent, so partition succeeds with leftover atoms in parent only.

sig BParent {}
sig BChild1 extends BParent {}
sig BChild2 extends BParent {}

test expect B_UnderPartition {
    -- Children can be populated
    b_basic: { some BChild1 and some BChild2 }
        for 6 BParent, 2 BChild1, 2 BChild2 is sat

    -- Parent has atoms beyond children (6 - 2 - 2 = 2 slack)
    b_parent_private: { #{BParent} > add[#{BChild1}, #{BChild2}] }
        for 6 BParent, 2 BChild1, 2 BChild2 is sat

    -- Children at full scope
    b_full: { #BChild1 = 2 and #BChild2 = 2 }
        for 6 BParent, 2 BChild1, 2 BChild2 is sat

    -- Parent can reach full scope
    b_parent_full: { #BParent = 6 }
        for 6 BParent, 2 BChild1, 2 BChild2 is sat
}

// ---- Section C: Over-committed ----
// Children sum > parent → partition skipped (all children share all parent atoms).

abstract sig CParent {}
sig CChild1 extends CParent {}
sig CChild2 extends CParent {}

test expect C_OverCommitted {
    -- Can have some of each (despite over-commitment)
    c_partial: { some CChild1 and some CChild2 }
        for 5 CParent, 3 CChild1, 3 CChild2 is sat

    -- Can't have both at max (3+3=6 > 5)
    c_full_unsat: { #CChild1 = 3 and #CChild2 = 3 }
        for 5 CParent, 3 CChild1, 3 CChild2 is unsat
}

// ---- Section D: `one sig` in hierarchy ----
// Mixed one/normal children. `one sig` has need=0 (lower already fills scope).

abstract sig DParent {}
one sig DOne extends DParent {}
sig DNormal extends DParent {}

test expect D_OneSig {
    -- DOne is always exactly 1
    d_one_exact: { #DOne != 1 }
        for 4 DParent, 3 DNormal is unsat

    -- DNormal can reach full scope
    d_normal_full: { #DNormal = 3 }
        for 4 DParent, 3 DNormal is sat

    -- Total: one(1) + normal(3) = 4 = parent scope → perfect fit
    d_perfect: { #DParent = 4 }
        for 4 DParent, 3 DNormal is sat

    -- Disjointness
    d_disjoint: { some (DOne & DNormal) }
        for 4 DParent, 3 DNormal is unsat
}

// ---- Section E: `lone sig` in hierarchy ----
// Lone child has need = max(0, 1-0) = 1 (upper=1, lower=0).

abstract sig EParent {}
lone sig ELone extends EParent {}
sig ENormal extends EParent {}

test expect E_LoneSig {
    -- ELone is 0 or 1
    e_lone_zero: { no ELone }
        for 4 EParent, 3 ENormal is sat

    e_lone_one: { one ELone }
        for 4 EParent, 3 ENormal is sat

    e_lone_more: { #ELone > 1 }
        for 4 EParent, 3 ENormal is unsat

    -- ENormal at full scope
    e_normal_full: { #ENormal = 3 }
        for 4 EParent, 3 ENormal is sat

    -- Both at max: lone(1) + normal(3) = 4 = parent
    e_both_max: { one ELone and #ENormal = 3 }
        for 4 EParent, 3 ENormal is sat
}

// ---- Section F: Multi-level (3 levels) ----
// Nested hierarchy: Root → Mid → Leaf. Partition should apply at each level.

abstract sig FRoot {}
abstract sig FMid extends FRoot {}
sig FLeaf1 extends FMid {}
sig FLeaf2 extends FMid {}
sig FOther extends FRoot {}

test expect F_MultiLevel {
    -- Basic: all levels populated
    f_basic: { some FLeaf1 and some FLeaf2 and some FOther }
        for 6 FRoot, 4 FMid, 2 FLeaf1, 2 FLeaf2, 2 FOther is sat

    -- Leaves at full scope
    f_full: { #FLeaf1 = 2 and #FLeaf2 = 2 and #FOther = 2 }
        for 6 FRoot, 4 FMid, 2 FLeaf1, 2 FLeaf2, 2 FOther is sat

    -- Mid = union of leaves (abstract)
    f_mid_abstract: { FMid = FLeaf1 + FLeaf2 }
        for 6 FRoot, 4 FMid, 2 FLeaf1, 2 FLeaf2, 2 FOther is sat

    -- Disjointness at both levels
    f_disjoint_leaves: { some (FLeaf1 & FLeaf2) }
        for 6 FRoot, 4 FMid, 2 FLeaf1, 2 FLeaf2, 2 FOther is unsat

    f_disjoint_branches: { some (FMid & FOther) }
        for 6 FRoot, 4 FMid, 2 FLeaf1, 2 FLeaf2, 2 FOther is unsat
}

// ---- Section G: Fields on hierarchy sigs ----
// Relations involving partitioned sigs. Field upper bounds must be correct.

abstract sig GParent {}
sig GChild1 extends GParent { glink: lone GParent }
sig GChild2 extends GParent {}

test expect G_FieldsOnHierarchy {
    -- Field from child to parent works
    g_basic: { some glink }
        for 4 GParent, 2 GChild1, 2 GChild2 is sat

    -- Self-link within same child
    g_self: { some c: GChild1 | c.glink = c }
        for 4 GParent, 2 GChild1, 2 GChild2 is sat

    -- Cross-sibling link: GChild1 → GChild2
    g_cross: { some c1: GChild1 | some c2: GChild2 | c1.glink = c2 }
        for 4 GParent, 2 GChild1, 2 GChild2 is sat

    -- All GChild1 instances have links
    g_all_linked: { all c: GChild1 | some c.glink }
        for 4 GParent, 2 GChild1, 2 GChild2 is sat
}

// ---- Section H: Cross-partition fields ----
// Relation between siblings in different partition slices.

abstract sig HKey {}
sig HPriv extends HKey {}
sig HPub extends HKey {
    hpair: lone HPriv
}

test expect H_CrossPartitionFields {
    -- Pairing between siblings works
    h_basic: { some hpair }
        for 6 HKey, 3 HPriv, 3 HPub is sat

    -- Every public key has a private pair
    h_all_paired: { all pk: HPub | some pk.hpair }
        for 6 HKey, 3 HPriv, 3 HPub is sat

    -- Injective pairing (each private key paired with at most one public)
    h_injective: { all pk1, pk2: HPub | pk1 != pk2 implies pk1.hpair != pk2.hpair }
        for 6 HKey, 3 HPriv, 3 HPub is sat

    -- Equal counts with full pairing
    h_full_pairing: { #HPriv = 3 and #HPub = 3 and (all pk: HPub | some pk.hpair) }
        for 6 HKey, 3 HPriv, 3 HPub is sat
}

// ---- Section I: Sibling disjointness under partition ----
// Explicit verification that sibling sigs remain disjoint with tighter bounds.

abstract sig ISig {}
sig ISib1 extends ISig {}
sig ISib2 extends ISig {}
sig ISib3 extends ISig {}

test expect I_SiblingDisjointness {
    -- No overlap between any pair
    i_12: { some (ISib1 & ISib2) }
        for 6 ISig, 2 ISib1, 2 ISib2, 2 ISib3 is unsat

    i_13: { some (ISib1 & ISib3) }
        for 6 ISig, 2 ISib1, 2 ISib2, 2 ISib3 is unsat

    i_23: { some (ISib2 & ISib3) }
        for 6 ISig, 2 ISib1, 2 ISib2, 2 ISib3 is unsat

    -- But each can be populated
    i_all: { some ISib1 and some ISib2 and some ISib3 }
        for 6 ISig, 2 ISib1, 2 ISib2, 2 ISib3 is sat

    -- Full population
    i_full: { #ISib1 = 2 and #ISib2 = 2 and #ISib3 = 2 }
        for 6 ISig, 2 ISib1, 2 ISib2, 2 ISib3 is sat
}

// ---- Section J: inst bounds coexistence ----
// Already covered in detail by temp-bounds-perf/inst-scope-baseline.frg.
// Minimal regression tests here for the key cases.

// J-a: Independent hierarchies
sig JaAlpha {}
sig JaBeta extends JaAlpha {}
sig JaGamma extends JaAlpha {}
sig JaOther {}

inst instJaOther { JaOther = `JaOther0 + `JaOther1 }

test expect Ja_IndependentHierarchies {
    ja_basic: { some JaAlpha and some JaOther }
        for 4 JaAlpha, 2 JaBeta, 2 JaGamma for instJaOther is sat

    ja_other_fixed: { #JaOther != 2 }
        for 4 JaAlpha, 2 JaBeta, 2 JaGamma for instJaOther is unsat
}

// J-b: Relation-only inst referencing surplus atoms
sig JbParent {}
sig JbChildA extends JbParent { jblink: lone JbParent }
sig JbChildB extends JbParent {}

inst instJbRelOnly { jblink = `JbParent0 -> `JbParent0 }

test expect Jb_RelationOnlyInst {
    jb_basic: { some jblink }
        for 4 JbParent, 2 JbChildA, 2 JbChildB for instJbRelOnly is sat

    jb_children: { #JbChildA >= 1 and #JbChildB >= 1 }
        for 4 JbParent, 2 JbChildA, 2 JbChildB for instJbRelOnly is sat
}

// J-d: Lower-bound atoms (one sig)
sig JdVehicle {}
one sig JdCar extends JdVehicle {}
sig JdTruck extends JdVehicle {}
sig JdConnector { jdtarget: lone JdVehicle }

inst instJdOneSig { jdtarget = `JdConnector0 -> `JdCar0 }

test expect Jd_LowerBoundAtoms {
    jd_basic: { some jdtarget }
        for 4 JdVehicle, 3 JdTruck, 2 JdConnector for instJdOneSig is sat

    jd_truck_full: { #JdTruck = 3 }
        for 4 JdVehicle, 3 JdTruck, 2 JdConnector for instJdOneSig is sat
}

// ---- Section K: Non-abstract parent with abstract child ----
// Mixed abstract/non-abstract in same hierarchy.

sig KParent {}
abstract sig KAbsChild extends KParent {}
sig KGrand1 extends KAbsChild {}
sig KGrand2 extends KAbsChild {}

test expect K_MixedAbstract {
    -- Parent can have atoms beyond the abstract child
    k_parent_private: { #{KParent} > #{KAbsChild} }
        for 6 KParent, 4 KAbsChild, 2 KGrand1, 2 KGrand2 is sat

    -- Abstract child = union of grandchildren
    k_abs: { KAbsChild = KGrand1 + KGrand2 }
        for 6 KParent, 4 KAbsChild, 2 KGrand1, 2 KGrand2 is sat

    -- Grandchildren at full scope
    k_full: { #KGrand1 = 2 and #KGrand2 = 2 }
        for 6 KParent, 4 KAbsChild, 2 KGrand1, 2 KGrand2 is sat
}

// ---- Section L: Fallback-then-partition interleaving ----
// At the A level, children over-commit → partition skipped.
// At the B level, children fit numerically, but since partition was
// skipped at A, the atoms passed down include lower-bound atoms from
// siblings, making partition unsafe at B as well.

abstract sig LA {}
abstract sig LB extends LA {}
one sig LC extends LB {}
sig LD extends LB {}
sig LE extends LA {}

test expect L_FallbackInterleaving {
    -- D can reach full scope (unsafe partition at B would incorrectly limit it)
    l_d_full: { #LD = 3 }
        for 6 LA, 5 LB, 3 LD, 2 LE is sat

    -- All sigs populated
    l_all: { some LC and some LD and some LE }
        for 6 LA, 5 LB, 3 LD, 2 LE is sat

    -- LC is always exactly 1
    l_one: { #LC != 1 }
        for 6 LA, 5 LB, 3 LD, 2 LE is unsat
}

// ---- Section M: Global default scope ----
// `run {} for N` with no per-sig scopes.

sig MParent {}
sig MChild1 extends MParent {}
sig MChild2 extends MParent {}

test expect M_GlobalDefaultScope {
    -- With global scope 5: children default to 4 each, sum > 5 → partition skipped
    -- Still sat since each child can use 0..4 atoms
    m_basic: { some MChild1 and some MChild2 }
        for 5 is sat

    -- With explicit parent scope matching children
    m_explicit: { #MChild1 = 2 and #MChild2 = 2 }
        for 4 MParent, 2 MChild1, 2 MChild2 is sat
}

// ---- Section N: Piecewise bounds on scope-based sigs ----
// Piecewise relation bounds coexist with scope-based path.

sig NParent {}
sig NChildA extends NParent { nlink: lone NParent }
sig NChildB extends NParent {}

inst instNPiecewise { `NParent0.nlink = `NParent0 }

test expect N_PiecewiseBounds {
    -- Piecewise bound works with scope-based hierarchy
    n_basic: { some nlink }
        for 4 NParent, 2 NChildA, 2 NChildB for instNPiecewise is sat

    -- Children still usable
    n_siblings: { #NChildA >= 1 and #NChildB >= 1 }
        for 4 NParent, 2 NChildA, 2 NChildB for instNPiecewise is sat

    -- Full scope
    n_full: { #NChildA = 2 and #NChildB = 2 }
        for 4 NParent, 2 NChildA, 2 NChildB for instNPiecewise is sat
}

// ---- Section O: 3-level — partition fires at root, independently skips at mid ----
// Root=6, Mid=3, Other=3: partition fires at root (3+3=6).
// Mid has LeafA=2, LeafB=2: partition skips at mid (2+2=4 > 3).
// The skip at mid is due to its own over-commitment, not cascaded from root.

abstract sig ORoot {}
abstract sig OMid extends ORoot {}
sig OLeafA extends OMid {}
sig OLeafB extends OMid {}
sig OOther extends ORoot {}

test expect O_FireThenSkip {
    -- Leaves can be populated (they share Mid's 3 atoms)
    o_basic: { some OLeafA and some OLeafB and some OOther }
        for 6 ORoot, 3 OMid, 2 OLeafA, 2 OLeafB, 3 OOther is sat

    -- Leaves can partially fill Mid (respecting Mid scope of 3)
    o_leaves_partial: { #OLeafA = 2 and #OLeafB = 1 }
        for 6 ORoot, 3 OMid, 2 OLeafA, 2 OLeafB, 3 OOther is sat

    -- Both leaves at max would exceed Mid's scope (2+2=4 > 3, Mid is abstract)
    o_leaves_exceed_mid: { #OLeafA = 2 and #OLeafB = 2 }
        for 6 ORoot, 3 OMid, 2 OLeafA, 2 OLeafB, 3 OOther is unsat

    -- Disjointness at root level (partition fires here)
    o_disjoint_root: { some (OMid & OOther) }
        for 6 ORoot, 3 OMid, 2 OLeafA, 2 OLeafB, 3 OOther is unsat

    -- Disjointness at mid level (partition skipped, but Kodkod enforces)
    o_disjoint_mid: { some (OLeafA & OLeafB) }
        for 6 ORoot, 3 OMid, 2 OLeafA, 2 OLeafB, 3 OOther is unsat

    -- Other at full scope alongside partitioned leaves
    o_other_full: { #OOther = 3 and some OLeafA }
        for 6 ORoot, 3 OMid, 2 OLeafA, 2 OLeafB, 3 OOther is sat

    -- Both leaves at full simultaneously uses all Mid atoms → Mid = LeafA + LeafB
    o_abstract_union: { OMid = OLeafA + OLeafB and #OLeafA = 2 and #OLeafB = 1 }
        for 6 ORoot, 3 OMid, 2 OLeafA, 2 OLeafB, 3 OOther is sat
}

// ---- Section P: All four sibling types under one parent ----
// one + lone + abstract-with-children + normal, all under one abstract parent.
// Partition at parent level: need = 0+1+4+2 = 7 ≤ 7 surplus → fires.
// Partition at abstract child level: 2+2=4 ≤ 4 → fires.

abstract sig PParent {}
one sig POne extends PParent {}
lone sig PLone extends PParent {}
abstract sig PAbs extends PParent {}
sig PGrand1 extends PAbs {}
sig PGrand2 extends PAbs {}
sig PNorm extends PParent {}

test expect P_KitchenSink {
    -- All sibling types populated
    p_basic: { some POne and some PLone and some PGrand1 and some PGrand2 and some PNorm }
        for 8 PParent, 4 PAbs, 2 PGrand1, 2 PGrand2, 2 PNorm is sat

    -- POne is exactly 1
    p_one_exact: { #POne != 1 }
        for 8 PParent, 4 PAbs, 2 PGrand1, 2 PGrand2, 2 PNorm is unsat

    -- PLone is 0 or 1
    p_lone_range: { #PLone > 1 }
        for 8 PParent, 4 PAbs, 2 PGrand1, 2 PGrand2, 2 PNorm is unsat

    -- Grandchildren at full scope
    p_grands_full: { #PGrand1 = 2 and #PGrand2 = 2 }
        for 8 PParent, 4 PAbs, 2 PGrand1, 2 PGrand2, 2 PNorm is sat

    -- Abstract child = union of grandchildren
    p_abs_union: { PAbs = PGrand1 + PGrand2 }
        for 8 PParent, 4 PAbs, 2 PGrand1, 2 PGrand2, 2 PNorm is sat

    -- All pairwise disjoint at parent level
    p_disj_one_abs: { some (POne & PAbs) }
        for 8 PParent, 4 PAbs, 2 PGrand1, 2 PGrand2, 2 PNorm is unsat

    p_disj_one_norm: { some (POne & PNorm) }
        for 8 PParent, 4 PAbs, 2 PGrand1, 2 PGrand2, 2 PNorm is unsat

    p_disj_lone_abs: { some (PLone & PAbs) }
        for 8 PParent, 4 PAbs, 2 PGrand1, 2 PGrand2, 2 PNorm is unsat

    p_disj_abs_norm: { some (PAbs & PNorm) }
        for 8 PParent, 4 PAbs, 2 PGrand1, 2 PGrand2, 2 PNorm is unsat

    -- Everything at max simultaneously
    p_max_all: { one PLone and #PGrand1 = 2 and #PGrand2 = 2 and #PNorm = 2 }
        for 8 PParent, 4 PAbs, 2 PGrand1, 2 PGrand2, 2 PNorm is sat
}

// ---- Section Q: 4-level hierarchy with varying partition decisions ----
// Root(12) → BranchA(7) → MidA(5) → SubA(3) → {LeafA(2), LeafB(2)}
//                        → SibA(2)  → SubB(3)
//          → BranchB(5)
// Fires at Root→children (7+5=12), fires at BranchA→children (5+2=7),
// SKIPS at MidA→children (3+3=6 > 5), cascade skips at SubA→children.

abstract sig QRoot {}
abstract sig QBrA extends QRoot {}
abstract sig QMidA extends QBrA {}
abstract sig QSubA extends QMidA {}
sig QLeafA extends QSubA {}
sig QLeafB extends QSubA {}
sig QSubB extends QMidA {}
sig QSibA extends QBrA {}
sig QBrB extends QRoot {}

test expect Q_DeepHierarchy {
    -- All levels populated
    q_basic: { some QLeafA and some QLeafB and some QSubB and some QSibA and some QBrB }
        for 12 QRoot, 7 QBrA, 5 QMidA, 3 QSubA, 2 QLeafA, 2 QLeafB,
            3 QSubB, 2 QSibA, 5 QBrB is sat

    -- Root-level disjointness (partition fires)
    q_disj_root: { some (QBrA & QBrB) }
        for 12 QRoot, 7 QBrA, 5 QMidA, 3 QSubA, 2 QLeafA, 2 QLeafB,
            3 QSubB, 2 QSibA, 5 QBrB is unsat

    -- BranchA-level disjointness (partition fires)
    q_disj_bra: { some (QMidA & QSibA) }
        for 12 QRoot, 7 QBrA, 5 QMidA, 3 QSubA, 2 QLeafA, 2 QLeafB,
            3 QSubB, 2 QSibA, 5 QBrB is unsat

    -- MidA-level disjointness (partition skips, but Kodkod still enforces)
    q_disj_mida: { some (QSubA & QSubB) }
        for 12 QRoot, 7 QBrA, 5 QMidA, 3 QSubA, 2 QLeafA, 2 QLeafB,
            3 QSubB, 2 QSibA, 5 QBrB is unsat

    -- SubA-level disjointness (cascade skip, Kodkod enforces)
    q_disj_suba: { some (QLeafA & QLeafB) }
        for 12 QRoot, 7 QBrA, 5 QMidA, 3 QSubA, 2 QLeafA, 2 QLeafB,
            3 QSubB, 2 QSibA, 5 QBrB is unsat

    -- Leaves can partially fill (respecting SubA scope of 3)
    q_leaves_partial: { #QLeafA = 2 and #QLeafB = 1 }
        for 12 QRoot, 7 QBrA, 5 QMidA, 3 QSubA, 2 QLeafA, 2 QLeafB,
            3 QSubB, 2 QSibA, 5 QBrB is sat

    -- Both leaves at max would exceed SubA's scope (2+2=4 > 3)
    q_leaves_exceed_sub: { #QLeafA = 2 and #QLeafB = 2 }
        for 12 QRoot, 7 QBrA, 5 QMidA, 3 QSubA, 2 QLeafA, 2 QLeafB,
            3 QSubB, 2 QSibA, 5 QBrB is unsat

    -- BranchB at full scope alongside deep partitioned hierarchy
    q_brb_full: { #QBrB = 5 }
        for 12 QRoot, 7 QBrA, 5 QMidA, 3 QSubA, 2 QLeafA, 2 QLeafB,
            3 QSubB, 2 QSibA, 5 QBrB is sat
}

// ---- Section R: Two independent top-level hierarchies (exact vs non-exact) ----
// Hierarchy 1: exact children. Hierarchy 2: non-exact children.
// Partition decisions on each should be independent.

abstract sig R1Root {}
sig R1A extends R1Root {}
sig R1B extends R1Root {}

abstract sig R2Root {}
sig R2A extends R2Root {}
sig R2B extends R2Root {}

test expect R_IndependentRoots {
    -- Both hierarchies populated
    r_basic: { some R1A and some R1B and some R2A and some R2B }
        for 6 R1Root, exactly 3 R1A, exactly 3 R1B, 6 R2Root, 3 R2A, 3 R2B is sat

    -- Exact hierarchy children at full scope
    r_exact_full: { #R1A = 3 and #R1B = 3 }
        for 6 R1Root, exactly 3 R1A, exactly 3 R1B, 6 R2Root, 3 R2A, 3 R2B is sat

    -- Non-exact hierarchy children at full scope
    r_nonexact_full: { #R2A = 3 and #R2B = 3 }
        for 6 R1Root, exactly 3 R1A, exactly 3 R1B, 6 R2Root, 3 R2A, 3 R2B is sat

    -- Cross-hierarchy disjointness (always holds for separate roots)
    r_cross_disj: { some (R1Root & R2Root) }
        for 6 R1Root, exactly 3 R1A, exactly 3 R1B, 6 R2Root, 3 R2A, 3 R2B is unsat

    -- Intra-hierarchy disjointness
    r_disj_h1: { some (R1A & R1B) }
        for 6 R1Root, exactly 3 R1A, exactly 3 R1B, 6 R2Root, 3 R2A, 3 R2B is unsat

    r_disj_h2: { some (R2A & R2B) }
        for 6 R1Root, exactly 3 R1A, exactly 3 R1B, 6 R2Root, 3 R2A, 3 R2B is unsat

    -- Both at full simultaneously
    r_both_full: { #R1A = 3 and #R1B = 3 and #R2A = 3 and #R2B = 3 }
        for 6 R1Root, exactly 3 R1A, exactly 3 R1B, 6 R2Root, 3 R2A, 3 R2B is sat
}
