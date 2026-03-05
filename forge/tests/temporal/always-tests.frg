#lang forge

option run_sterling off
option verbose 0
option problem_type temporal

sig Node {
    var edges : set Node
}

-- State machine: edges empty -> edges full -> stays full
pred fillOnce {
    no edges
    always (no edges implies Node->Node in edges')
    always (Node->Node in edges implies edges' = edges)
}

test expect alwaysBaseCases {
    -- always P requires P in the current state
    alwaysImpliesNow: {always some edges and no edges} is unsat
    -- always P requires P in the next state
    alwaysImpliesNext: {always some edges and next_state no edges} is unsat
    -- always P holds when P is true in every state
    alwaysTrivial: {always Node = Node} is sat
    -- always false is unsat (current state violates)
    alwaysFalseIsUnsat: {always false} is unsat
    -- always true is trivially sat
    alwaysTrueIsSat: {always true} is sat
}

var sig Token {}

pred tokenSwitch {
    -- Token alternates between empty and nonempty each step
    no Token implies some Token'
    some Token implies no Token'
    always (no Token implies some Token')
    always (some Token implies no Token')
}

test expect alwaysVsNextStateAlways {
    -- always includes the current state; next_state always does not
    alwaysIncludesCurrent: {
        tokenSwitch
        some Token
        always some Token
    } is unsat  -- fails because next state has no Token

    nextStateAlwaysExcludesCurrent: {
        tokenSwitch
        no Token
        next_state always some Token
    } is unsat  -- fails because Token alternates

    -- If P holds now but not always, next_state always P can still fail
    alwaysStrongerThanNow: {
        tokenSwitch
        some Token
        not always some Token
    } is sat
}

test expect alwaysWithNegation {
    -- not always P means P fails at some point
    notAlwaysMeansSomeFail: {
        fillOnce
        not always (no edges)
    } is sat  -- edges become full eventually

    -- not always P is equivalent to eventually not P
    notAlwaysIsEventuallyNot: {
        not always some Token
        not (eventually no Token)
    } is unsat

    eventuallyNotIsNotAlways: {
        eventually no Token
        not (not always some Token)
    } is unsat
}

test expect alwaysNesting {
    -- always always P is equivalent to always P
    doubleAlwaysForward: {
        always always (Node = Node)
        not always (Node = Node)
    } is unsat

    doubleAlwaysBackward: {
        always (Node = Node)
        not always always (Node = Node)
    } is unsat
}

test expect alwaysEventuallyDuality {
    -- always P iff not eventually not P
    alwaysImpliesNotEventuallyNot: {
        always some Token
        eventually no Token
    } is unsat

    notEventuallyNotImpliesAlways: {
        not eventually no Token
        not always some Token
    } is unsat

    -- eventually P iff not always not P
    eventuallyImpliesNotAlwaysNot: {
        eventually some Token
        always no Token
    } is unsat

    notAlwaysNotImpliesEventually: {
        not always no Token
        not eventually some Token
    } is unsat
}

test expect alwaysWithTransitions {
    -- fillOnce: edges start empty, become full, stay full
    -- so "always (Node->Node in edges)" is false (fails in initial state)
    notAlwaysFullFromStart: {
        some Node
        fillOnce
        always (Node->Node in edges)
    } is unsat

    -- but next_state always full should hold
    alwaysFullAfterFill: {
        some Node
        fillOnce
        next_state always (Node->Node in edges)
    } is sat

    alwaysFullAfterFillMust: {
        some Node
        fillOnce
        not (next_state always (Node->Node in edges))
    } is unsat
}
