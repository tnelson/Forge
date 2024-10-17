#lang froglet

option backend smtlibtor

abstract sig Person { 
    time: one Int,
    shore: func State -> Position
}
one sig A extends Person {}
one sig B extends Person {}
one sig C extends Person {}
one sig D extends Person {}

abstract sig Position {}
one sig Near extends Position {}
one sig Far extends Position {}

sig State {
    next: lone State,  
    torch: one Position,
    spent: one Int
}

pred ValidStates {
    all s: State, p: Person | {
        p.shore[s] = Near or p.shore[s] = Far
    }
    all s: State {
        s.torch = Near or s.torch = Far
    }
}

pred initState[s: State] {
    all p: Person | p.shore[s] = Near
    s.torch = Near
    s.spent = 0
}

pred finalState[s: State] {
    all p: Person | p.shore[s] = Far
    s.spent <= 15
}

pred canTransition[pre: State, post: State] {
    pre.torch != post.torch
    some p1, p2: Person | {
        p1.shore[pre] = pre.torch
        p2.shore[pre] = pre.torch
        p1.shore[post] = post.torch
        p2.shore[post] = post.torch
        all p: Person | {
            (p != p1 and p != p2) iff p.shore[pre] = p.shore[post]
        }
        p1.time >= p2.time => {
            post.spent = add[pre.spent, p1.time]
        } else {
            post.spent = add[pre.spent, p2.time]
        }
    }
}

pred TransitionStates {
    some init, final: State {
        initState[init]
        no s: State | s.next = init
        finalState[final]
        no final.next
        all s : State | (s != init) => reachable[s, init, next]
        all s: State | s != final => canTransition[s, s.next]
    }
}

pred stateExists { 
    some s : State | {
        s.spent = -1
    }
}

pred Speeds {
    A.time = 1
    B.time = 2
    C.time = 5
    D.time = 8
}

test expect {
    crossing : { ValidStates and TransitionStates and Speeds } for 6 State is sat
}





