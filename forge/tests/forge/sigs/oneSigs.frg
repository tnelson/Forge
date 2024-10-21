#lang forge

option run_sterling off


option verbose 0

one sig UniqueObject {}

sig Stuff {}

test expect {
    oneSigEnforced : { #UniqueObject = 1 } is checked
    oneSigIsntPersistent : { #Stuff = 2 } is sat
}


sig Thing {}
one sig SpecialThing extends Thing {}
sig UnspecialThing extends Thing {}

test expect oneExtending {
    oneExtendActuallyExtends : { SpecialThing in Thing } is checked
    oneExtendEnforced : { #SpecialThing = 1 } is checked
    oneExtendDoesntSpread : { #UnspecialThing = 2 } is sat
}
