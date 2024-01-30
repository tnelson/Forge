#lang forge

option run_sterling off
option verbose 0

sig BasicA {
    friendA: set BasicA
}

sig BasicB {
    friendB: set BasicA->BasicB
}

-- Empty Inst
inst emptyInst {}
test expect {
    emptyInstSat : {} for emptyInst is sat
    emptyInstIsPartial : { #BasicA = 1 } for emptyInst is sat
    emptyInstExact : { #BasicA = 1 } for exactly emptyInst is sat
}

-- Basic Insts
inst basicInst {
    BasicA = `BasicA1 + `BasicA2 + `BasicA3
    BasicB = `BasicB1 + `BasicB2 + `BasicB3

    friendA = `BasicA1->`BasicA1 + `BasicA2->`BasicA2 + `BasicA3->`BasicA3
    friendB = `BasicB1->`BasicA2->`BasicB3 + `BasicB2->`BasicA3->`BasicB2
}

inst basicInst_froglet_syntax_permissive {
    BasicA = `BasicA1 + `BasicA2 + `BasicA3
    BasicB = `BasicB1 + `BasicB2 + `BasicB3

    friendA = (`BasicA1->`BasicA1) + `BasicA2->`BasicA2 + `BasicA3->`BasicA3
    -- Abuse of the syntax, but checking for permissiveness
    friendB = (`BasicB1,`BasicA2)->`BasicB3 + 
              (`BasicB2->`BasicA3,`BasicB2)
}


test expect Basic {
    satsifiableInstBasic : {} for basicInst is sat
    sigsCorrect : { not (#BasicA = 3 and #BasicB = 3) } for basicInst is unsat
     relationsCorrect : { not (
             (friendA = BasicA->BasicA & iden) and 
             (#friendB = 2))
         } for basicInst is unsat
}

-- Empty Sig Insts
inst emptySigInst {
    no BasicA
    BasicB = `BasicB1 + `BasicB2 + `BasicB3

    no friendA
    no friendB
}

test expect EmptySig {
    satisfiableInstEmptySig : {} for emptySigInst is sat
    sigIsEmpty : { some BasicA } for emptySigInst is unsat
    otherSigCorrect : { #BasicB != 3 } for emptySigInst is unsat
}

-- Empty Relation Insts
inst emptyRelationInst {
    BasicA = `BasicA1 + `BasicA2 + `BasicA3
    BasicB = `BasicB1 + `BasicB2 + `BasicB3

    friendA = `BasicA1->`BasicA1 + `BasicA2->`BasicA2 + `BasicA3->`BasicA3
    no friendB
}

test expect EmptyRelation {
    satisfiableInstEmptyRelation : {} for emptyRelationInst is sat
    relationIsEmpty : { some friendB } for emptyRelationInst is unsat
}

-- Partial Insts
inst partialInst1 {
    BasicA = `BasicA1 + `BasicA2 + `BasicA3

    friendA = `BasicA1->`BasicA1 + `BasicA2->`BasicA2 + `BasicA3->`BasicA3
}

test expect Partial {
    satisfiableInstPartial : {} for partialInst1 is sat
    specifiedSigRestricted : { #BasicA != 3 } for partialInst1 is unsat
    unspecifiedSigUnrestricted : { some BasicB } for partialInst1 is sat
    specifiedRelRestricted : { #friendA != 3 } for partialInst1 is unsat
    unspecifiedRelUnrestricted : { some friendB } for partialInst1 is sat
}


-- Insts extending other Insts
inst partialInst2 {
    BasicB = `BasicB1 + `BasicB2 + `BasicB3
}

inst jointInst {
    partialInst1
    partialInst2
}

test expect ExtendingInsts {
    satisfiableExtendingInsts : {} for jointInst is sat
    sigsDefinedJoint : { not (#BasicA = 3 and #BasicB = 3) } for jointInst is unsat
    specifiedRelRestrictedJoint : { #friendA != 3 } for jointInst is unsat
    unspecifiedRelUnrestrictedJoint : { some friendB } for jointInst is sat
}


-- Extending sigs works with Insts
sig ToExtend {}
sig AnExtension extends ToExtend {}
sig AnotherExtension extends ToExtend {}

inst extendSigInst {
    AnExtension = `Atom1
    AnotherExtension = `Atom3
    ToExtend = `Atom1 + `Atom2 + `Atom3
}

test expect ExtendingSigs {
    extendingSigsWorks : {} for extendSigInst is sat
}


-- one sigs work with Insts
one sig OneSig {}

inst oneSigInst {
    OneSig = `OneSig1
}

test expect OneSigs {
    oneSigWorks : {} for oneSigInst is sat
}


-- abstract sig works with Insts
abstract sig Abstract {}
sig AbstractExtension1 extends Abstract {}
sig AbstractExtension2 extends Abstract {}

inst abstractSigInst {
    AbstractExtension1 = `Atom1 + `Atom2
    AbstractExtension2 = `Atom3 + `Atom4
    Abstract = `Atom1 + `Atom2 + `Atom3 + `Atom4
}

test expect AbstractSigs {
    abstractSigWorks : {} for abstractSigInst is sat
}


-- exactly keyword with Insts
-- You have to specify every sig, but relations inferred to be emptyInst
--   if one of columns is specified as empty
inst exactInst {
    BasicA = `BasicA1 + `BasicA2 + `BasicA3
    no BasicB
    friendA = `BasicA1->`BasicA2 + `BasicA2->`BasicA3 + `BasicA3->`BasicA1
    -- no friendB

    AnExtension = `Atom1
    AnotherExtension = `Atom3
    ToExtend = `Atom1 + `Atom2 + `Atom3

    OneSig = `OneSig1

    AbstractExtension1 = `Atom1 + `Atom2
    AbstractExtension2 = `Atom3 + `Atom4
    Abstract = `Atom1 + `Atom2 + `Atom3 + `Atom4
}

test expect ExactlyKeyword {
    exactlyKeywordWorks : {} for exactly exactInst is sat
}

-- Test that "ni" and "in" can be combined in complete bounds 
inst both_lower_and_upper {
    #Int = 4
    BasicA in `A0 + `A1 + `A2 + `A3 + `A4 
    BasicA ni `A0
}

test expect {
    mixed_complete_ni: {no BasicA} for both_lower_and_upper is unsat
    mixed_complete_in: {#BasicA = 5} for both_lower_and_upper is sat
}
