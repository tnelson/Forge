#lang forge

option run_sterling off


option verbose 0

sig A {
    otherA: one A
}

sig B {
    otherB: one A
}

pred ComprehensionsOnSigs {
    {a: A | a->a in otherA} = A.(otherA & iden)
}

pred ComprehensionsOnSets {
    {x: A + B | some y: A | x.otherA = y or x.otherB = y} = (otherA + otherB).A
}

pred MultiComprehension {
    {b: B, a: A | some c: A | b->c in otherB and c->a in otherA} = otherB.otherA
}

test expect {
    comprehensionsOnSigs : ComprehensionsOnSigs is checked
    comprehensionsOnSets : ComprehensionsOnSets is checked
    multiComprehension : MultiComprehension is checked
}
