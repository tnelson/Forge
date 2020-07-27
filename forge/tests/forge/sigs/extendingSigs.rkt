#lang forge

sig ToExtend {}
sig Extension1 extends ToExtend {}
sig Extension2 extends ToExtend {}
sig Extension3 extends Extension2 {}

test expect ExtensionSigs {
    extensionEnforced : { not ((Extension1 + Extension2) in ToExtend) } is unsat
    multipleExtensions : { some Extension1 and some Extension2 } is sat
    extensionsDisjoint : { some (Extension1 & Extension2) } is unsat
    extensionsNotExhaustive : { some (ToExtend - Extension1 - Extension2) } is sat
    doubleExtendingWorks : { not (Extension3 in Extension2 and Extension2 in ToExtend) } is unsat
}


sig Parent {
	parentRel : set Child
}

sig Child extends Parent {
	childRel : set Parent
}

test expect ExtensionRelations {
	relationsIntoExtension : { not (parentRel[Parent] in Child) } is unsat
	extensionsInheritRelations : { some parentRel[Child] } is sat
	parentsDontGetExtensionRelations : { not (childRel.Parent in Child) } is unsat
}