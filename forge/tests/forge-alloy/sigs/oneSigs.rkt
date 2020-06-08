#lang forge

one sig UniqueObject {}

sig Stuff {}

test expect {
    oneSigEnforced : { #UniqueObject != 1 } is unsat
    oneSigIsntPersistent : { #Stuff = 2 } is sat
}


sig Thing {}
one sig SpecialThing extends Thing {}
sig unspecialThing extends Thing {}

test expect oneExtending {
    oneExtendActuallyExtends : { SpecialThing !in Thing } is unsat
    oneExtendEnforced : { #SpecialThing != 1 } is unsat
    oneExtendDoesntSpread : { #UnspecialThing = 2 } is sat
}