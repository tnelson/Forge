#lang forge

sig A {}
sig State {
    contents: A
}
pred initPred[s: State] {
	no s.contents
}
pred termPred[s: State] {
	s.contents = A
}
pred tranPred[s: State, s': State, a: A] {
    one a
    s.contents + a = s'.contents
    a not in s.contents
}

----

sig Sol {
    tran: State->State,
    init, term: State,
    argA: State->A
}
fact tran: linear
pred Sol_Facts {
    -- init
    one init
    initPred[Sol.init]
    no (Sol.tran) . (Sol.init)
    -- term
    one term
    termPred[Sol.term]
    no (Sol.term) . (Sol.tran)
    -- tran
    all s: State {
        --s = Sol.term or 
        tranPred[s, s.(Sol.tran), s.(Sol.argA)]
    }
}

run {Sol_Facts} for exactly 1 Sol, exactly 4 State, exactly 3 A