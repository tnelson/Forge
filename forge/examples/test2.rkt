#lang forge


sig U {}
sig A extends U {}
sig B extends U {}
sig C extends U {}
sig S { stuff: set U }

/**********/

transition[S] addA[a:A] {
    stuff' = stuff+a
    a not in stuff
}
transition[S] addBC[b:B, c:C] {
    stuff' = stuff+b+c
    b not in stuff
    c not in stuff
}

/**********/

state[S] S_init { no stuff }
transition[S] S_tran {
    (some a: A | addA[this, this', a]) or
    (some b: B | some c: C | addBC[this, this', b, c])
}
state[S] S_term { #stuff = 3 }

one sig T {
    tran: set S->S
}
fact tran: tree
facts[T] T_facts {
    all s: S {
        (no tran.s) => S_init[s]
        all s': s.tran { S_tran[s, s'] }
        (no s.tran) => S_term[s]
    }
}

/**********/

pred clean {
    U = A+B+C
    S.stuff = U
}

run {
    all t: T | T_facts[t]
    clean
} for exactly 4 S






