#lang forge

option run_sterling off


option verbose 0

sig ToExtend {}
sig Extension1 extends ToExtend {}
sig Extension2 extends ToExtend {}
sig Extension3 extends Extension2 {}

test expect ExtensionSigs {
    extensionEnforced : { (Extension1 + Extension2) in ToExtend } is checked
    multipleExtensions : { some Extension1 and some Extension2 } is sat
    extensionsDisjoint : { no (Extension1 & Extension2) } is checked
    extensionsNotExhaustive : { some (ToExtend - Extension1 - Extension2) } is sat
    doubleExtendingWorks : { Extension3 in Extension2 and Extension2 in ToExtend } is checked
}


sig Parent {
	parentRel : set Child
}

sig Child extends Parent {
	childRel : set Parent
}

test expect ExtensionRelations {
	relationsIntoExtension : { parentRel[Parent] in Child } is checked
	extensionsInheritRelations : { some parentRel[Child] } is sat
	parentsDontGetExtensionRelations : { childRel.Parent in Child } is checked
}
