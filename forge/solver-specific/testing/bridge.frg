#lang froglet

option backend smtlibtor 

option verbose 0

abstract sig Person { 
    time: one Int,  -- how long it takes this person to cross
    shore: func State -> Position -- which shore are they on in each State?
}
one sig A extends Person {}
one sig B extends Person {}
one sig C extends Person {}
one sig D extends Person {}

abstract sig Position {}
one sig Near extends Position {}
one sig Far extends Position {}

sig State {
    next: lone State, -- every state has at most one next state    
    torch: one Position, -- the position of the torch in this state    
    spent: one Int -- total time spent so far
}

pred ValidStates {
    // TODO: Fill me in!
    all s: State, p: Person | {
        p.shore[s] = Near or p.shore[s] = Far
    }
    all s: State {
        s.torch = Near or s.torch = Far
    }
}

pred initState[s: State] {
    // TODO: Fill me in!
    all p: Person | p.shore[s] = Near
    s.torch = Near
    s.spent = 0
}

pred finalState[s: State] {
    // TODO: Fill me in!

    // Everyone should get across before the 
    // bridge falls after 15 minutes!
    all p: Person | p.shore[s] = Far
    s.spent <= 15
}

pred canTransition[pre: State, post: State] {
    // TODO: Fill me in!

    // If two people cross the bridge, they can 
    // only travel as fast as the slower of the two.
    -- the torch must move
    pre.torch != post.torch

    -- exactly 1 or 2 people cross
    -- note that 1 person crosses iff a1 == a2 and 2 people cross iff a1 != a2
    some p1: Person | {
        some p2: Person | {
            -- one/two people start on the same side as the torch
            p1.shore[pre] = pre.torch
            p2.shore[pre] = pre.torch
            -- these same one/two people follow the torch to the other side
            p1.shore[post] = post.torch
            p2.shore[post] = post.torch
            -- every other person stays in place
            all p: Person | {
                (p != p1 and p != p2) iff p.shore[pre] = p.shore[post]
            }
            -- can only travel as fast as the slowest person
            p1.time >= p2.time => {
                post.spent = add[pre.spent, p1.time]
            } else {
                post.spent = add[pre.spent, p2.time]
            }
        }
    }
}

pred TransitionStates {
    // TODO: Fill me in!
    some init: State | {
        some final: State | {
            -- an initial state
            initState[init]
            -- no state before the init state
            no s: State | s.next = init

            -- a final state
            finalState[final]
            -- the final state has no next state
            no final.next

            -- every state is reachable from the initial state
            all s : State | (s != init) => reachable[s, init, next]

            -- all of the state transitions are valid
            all s: State | s != final => canTransition[s, s.next]
        }
    }
}

-----------------------------------------

// If you decide to play around with these (for fun!),
// make sure you return them to normal before submitting!
pred Speeds {
    A.time = 1
    B.time = 2
    C.time = 5
    D.time = 8
}

run {
    ValidStates
    TransitionStates
    Speeds
} for exactly 6 State


// FOR AFTER YOU FINISH:

// For up to what number of States would you have to check
// to verify the claim that all solutions have exactly 6 states?
// How would you write this as a test?
// (This is not a TODO, but something fun to think about!)

// Note: If you try setting State above 10, you will likely 
//       have to increase your bitwidth to 8. 
//       Try uncommenting the below run to see why!

/*
run {
    ValidStates
    TransitionStates
    Speeds
} for exactly 10 State, 7 Int for { next is linear }
*/




