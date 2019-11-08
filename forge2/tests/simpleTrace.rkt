#lang forge

sig A {}
sig S { stuff: A }

pred myInit[s: S] { no s.stuff }
pred myTrans[s: S, s': S, a: A] {
    s'.stuff = s.stuff+a
    a not in s.stuff
}
pred myTerm[s: S] { s.stuff = A }

one sig Solution {
    state: S,
    init, term: state,
    transition: (state-term) one->one (state-init), // one->one
    argA: (state-term) -> A
} {
    myInit[init]
    all s: (state-term) {
        myTrans[s, s.transition, s.argA]
    }
    myTerm[term]
}

run {} for 3 S, exactly 2 A