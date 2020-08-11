#lang forge

one sig UniqueObject {}

sig Stuff {}

test expect {
    oneSigEnforced : { #UniqueObject = 1 } is theorem
    oneSigIsntPersistent : { #Stuff = 2 } is sat
}


sig Thing {}
one sig SpecialThing extends Thing {}
sig UnspecialThing extends Thing {}

test expect oneExtending {
    oneExtendActuallyExtends : { SpecialThing in Thing } is theorem
    oneExtendEnforced : { #SpecialThing = 1 } is theorem
    oneExtendDoesntSpread : { #UnspecialThing = 2 } is sat
}
