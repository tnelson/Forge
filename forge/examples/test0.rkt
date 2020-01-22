#lang forge

sig A {}
sig State {
    stuff: set A
}
one sig Sol {
    r: set State->State
}
fact r: linear

state[State] inv {
--    this.@stuff = stuff
    some stuff
}
transition[State] tran {
--    this.@stuff = stuff
--    this'.@stuff = stuff'
    stuff in stuff'
    one stuff'-stuff
}

loner: A = A-Sol.r.State.stuff

run {
    all s: State {
        inv[s]
        all s': s.(Sol.r) | tran[s, s']
    }
}





--fact r: func
--run {
--    no r & iden
--    some A
--    all a:A {}
--} for 16 A

--fun arrr[a: A] : A {
--    a.r.r.r
--}
--fun f[a: A] : A {
--    let x = arrr[A] | let y = arrr[x] | A-y
--}
--query: A = f[A]
--pred p {
--    some A
--    all a:A { arrr[a] = a }
--}
--run {p} for 3 A
