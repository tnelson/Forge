#lang forge

sig A {}
sig S { stuff: A }

pred myInit[s: S] { no s.stuff }
pred myTrans[s: S, s': S, a: A] {
    s'.stuff = s.stuff+a
    a not in s.stuff
}
pred myTerm[s: S] { s.stuff = A }

one sig Sol {
    state: S,
    init, term: S//,
    //transition: S->S
  //  argA: (state-term) -> A
} /*{
    myInit[init]
    all s: (state-term) {
        myTrans[s, s.transition, s.argA]
    }
    myTerm[term]
}*/

pred facts {
    Sol.init in Sol.state
    Sol.term in Sol.state
    //(state-term) one->one (state-init)
    /*some transition: S->S {
        some transition
    }*/
}

run facts for 3 S, exactly 2 A