#lang forge

option run_sterling off


option verbose 0

lone sig MaybeObject {}

sig Stuff {}

test expect {
    loneSigEnforced : { one MaybeObject or no MaybeObject } is checked
    loneSigPermissive1 : { no MaybeObject } is sat
    loneSigPermissive2 : { one MaybeObject } is sat    
    loneSigIsntPersistent : { #Stuff = 2 } is sat
}


sig Thing {}
lone sig SpecialPossibility extends Thing {}
sig UnspecialThing extends Thing {}

test expect loneExtending {
    loneExtendActuallyExtends : { SpecialPossibility in Thing } is checked
    loneExtenderSigEnforced : { one SpecialPossibility or no SpecialPossibility } is checked
    loneExtenderSigPermissive1 : { no SpecialPossibility } is sat
    loneExtenderSigPermissive2 : { one SpecialPossibility } is sat    
    loneExtendDoesntSpread : { #UnspecialThing = 2 } is sat
}
