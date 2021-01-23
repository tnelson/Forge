#lang forge

option verbose 0

abstract sig Abstract {}
sig Extension1 extends Abstract {}
sig Extension2 extends Abstract {}

test expect NormalAbstract {
    abstractEnforced : { some (Abstract - Extension1 - Extension2) } is unsat
    extensionsAllowed : { some Extension1 } is sat
    emptyExtensionsEmptyAbstract : { no (Extension1 + Extension2) and some Abstract } is unsat
}


abstract sig Unextended {}

test expect UnextendedAbstract {
    unextendedCanPopulate : { some Unextended } is sat
}
