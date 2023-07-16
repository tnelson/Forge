#lang forge

sig A {}
sig S { stuff: set A }

pred myInit[s: S] { no s.stuff }
pred myTran[s1: S, s2: S, a: A] {
    s2.stuff = s1.stuff+a
    a not in s1.stuff
}
pred myTerm[s: S] { s.stuff = A }

one sig Sol {
    init, term: one S,
    tran: set S->S,
    argA: set S->A
}

fact tran: plinear
pred faccs {
    myInit[Sol.init]
    no (Sol.tran).(Sol.init)
    myTerm[Sol.term]
    no (Sol.term).(Sol.tran)

    no (Sol.term).(Sol.argA)
    all s: S-(Sol.term) {
        one s.(Sol.argA)
        myTran[s, s.(Sol.tran), s.(Sol.argA)]
    }
}

run faccs

