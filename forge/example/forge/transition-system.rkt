#lang forge

sig State {
    nextState: set State,
    contains: set Contents
}


sig Contents {}
one sig InitContents extends Contents {}
one sig TermContents extends Contents {}


-- USER INPUT HERE
-- An init state contains InitContents
pred init[s: set State] {
    s.contains = InitContents
}

-- USER INPUT HERE
-- A term state contains TermContents
pred term[s: set State] {
    s.contains = TermContents
}

-- USER INPUT HERE
-- Transitions must change contents 
pred legalTransition[pre: set State, post: set State] {
    pre.contains != post.contains
}


-- Enforce above predicates
pred validConfiguration {
    -- let not implemented
    init[State - State.nextState]
    term[State - nextState.State]
    all pre: State, post: pre.nextState |
        legalTransition[pre, post]
}

-- Enforce structure on transition and contains
/* To be implemented :(
inst trans {
    nextState is linear
    contains is func
}

transitionRun : run {
    validConfiguration
} for 6 State, exactly 6 Contents for trans
*/
