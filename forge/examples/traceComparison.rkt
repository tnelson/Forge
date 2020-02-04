#lang forge

#lang banana

sig A {}
sig S { stuff: set A }

state[S] S_init { no stuff }
transition[S] S_tran[a:A] {
    stuff' = stuff+a
    a not in stuff
}
state[S] S_term { #stuff = 3 }

--[ EXPLICIT ARGS ]--

--one sig T {
--    tran: set S->S,
--    argA: set S->A    -- DIFF
--}
--fact tran: linear
--fact argA: func       -- DIFF
--facts[T] T_facts {
--    all s: S {
--        (no tran.s) => S_init[s]
--        all s': s.tran | S_tran[s, s', s.argA]    -- DIFF
--        (no s.tran) => S_term[s]
--    }
--}

--[ IMPLICIT ARGS ]--

one sig T {
    tran: set S->S
}
fact tran: linear
facts[T] T_facts {
    all s: S {
        no tran.s => S_init[s]
        all s': s.tran | some a: A | S_tran[s, s', a]    --DIFF
        no s.tran => S_term[s]
    }
}

one sig Q { a: one A }
facts[Q] Q_facts {
    let s = S-S.(T.tran) | let s' = s.(T.tran) {
        S_tran[s, s', a]
    }
}

--[ BOTH ]--

run {
    all t:T | T_facts[t]
    all q:Q | Q_facts[q]
    S.stuff = A    -- clean up
}