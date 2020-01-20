#lang forge

sig U {}
sig A extends U {}
sig B extends U {}
sig C extends U {}
sig S { stuff: set U }

/**********/

pred f[s:S, s':S, a:A] {
    (one a)
    s'.stuff = s.stuff+a
    a not in s.stuff
}
pred g[s:S, s':S, b:B, c:C] {
    pBC[b, c]
    s'.stuff = s.stuff+b+c
    b not in s.stuff
    c not in s.stuff
}
pred h[s:S, s':S, a:A, b:B, c:C] {
    sApBC[a, b, c]
    (one a)   implies f[s, s', a]
    pBC[b, c] implies g[s, s', b, c]
}

pred sApBC[a:A, b:B, c:C] {
    (lone a) (lone b) (lone c)
    (one a) implies (no b+c) else pBC[b, c]
}
pred pBC[b:B, c:C] {
    (one b) (one c)
}

/**********/

pred myInit[s:S] { no s.stuff }
pred myTran[s:S, s':S, a:A, b:B, c:C] { h[s, s', a, b, c] }
pred myTerm[s:S] { s.stuff = U }

one sig Sol {
    init, term: one S,
    tran: set S->S,
    -- args:
    argA: set S->A,
    argB: set S->B,
    argC: set S->C
}
fact tran: plinear
pred Sol_facts {
    -- init/term
    myInit[Sol.init]
    no  (Sol.tran).(Sol.init)
    myTerm[Sol.term]
    no  (Sol.term).(Sol.tran)
    -- tran
    S = (Sol.init).*(Sol.tran)
    (Sol.argA).A in S-(Sol.term)
    (Sol.argB).B in S-(Sol.term)
    (Sol.argC).C in S-(Sol.term)
    all s: S-(Sol.term) {
        myTran[s, s.(Sol.tran), s.(Sol.argA), s.(Sol.argB), s.(Sol.argC)]
    }
}

/**********/

pred clean {
    U = A+B+C
    #U = 3
}

run {Sol_facts clean}






