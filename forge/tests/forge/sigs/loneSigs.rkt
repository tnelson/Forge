#lang forge

option verbose 0

lone sig MaybeObject {}

sig Stuff {}

test expect {
    loneSigEnforced : { one MaybeObject or no MaybeObject } is theorem
    loneSigPermissive : { no MaybeObject } is sat
    loneSigPermissive : { one MaybeObject } is sat    
    loneSigIsntPersistent : { #Stuff = 2 } is sat
}


sig Thing {}
lone sig SpecialPossibility extends Thing {}
sig UnspecialThing extends Thing {}

test expect loneExtending {
    loneExtendActuallyExtends : { SpecialPossibility in Thing } is theorem
    loneExtenderSigEnforced : { one SpecialPossibility or no SpecialPossibility } is theorem
    loneExtenderSigPermissive : { no SpecialPossibility } is sat
    loneExtenderSigPermissive : { one SpecialPossibility } is sat    
    loneExtendDoesntSpread : { #UnspecialThing = 2 } is sat
}
