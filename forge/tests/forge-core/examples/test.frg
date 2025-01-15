#lang forge/froglet

abstract sig Person { 
    time: one Int,  -- how long it takes this person to cross
    shore: func Statee -> Position -- which shore are they on in each State?
}
one sig A extends Person {}
one sig B extends Person {}
one sig C extends Person {}
one sig D extends Person {}

abstract sig Position {}
one sig Near extends Position {}
one sig Far extends Position {}

sig Statee {
    next: lone Statee, -- every state has at most one next state    
    torch: one Position, -- the position of the torch in this state    
    spent: one Int -- total time spent so far
}

pred ValidStates {
    // TODO: Fill me in!
    all s: Statee, p: Person | {
        p.shore[s] = Near or p.shore[s] = Far
    }
    all s: Statee {
        s.torch = Near or s.torch = Far
    }
}

pred reachableState[init: Statee, s: Statee]{
    reachable[init, s, next]
}
