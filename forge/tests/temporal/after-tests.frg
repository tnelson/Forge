#lang forge

option run_sterling off


-- Note: Forge changed its next-state temporal keyword 
-- from "after" to "next_state" in 2022.

option problem_type temporal
option verbose 0

sig Node {
    var edges : set Node
}

//State 0 : some edges, but it's not Node->Node
//State 1 : no edges
//State 2+ : edges is Node->Node
pred baseProg {
    some edges
    not edges = Node->Node
    no edges'
    always no edges implies Node->Node in edges'
    always Node->Node in edges implies edges' = edges
}

test expect afterBaseCases {
    //after looks at the next state
    CanBeNoEdgesInNextState : {baseProg and next_state no edges} is sat
    MustBeNoEdgesInNextState : {baseProg and not (next_state no edges)} is unsat
    //after does not look at current state
    SomeEdgesInCurrentState : {baseProg and some edges} is sat
    NotSomeEdgesAfter : {baseProg and next_state some edges} is unsat
    //after does not look beyond the next state
    EdgesNotCompleteAfter : {baseProg and next_state Node->Node in edges} is unsat
    EdgesCompleteEventually : {baseProg and next_state next_state Node->Node in edges} is sat
}

var sig Table {}

test expect afterTrueOrFalse {
    noTableCanBecomeSomeTable : {no Table and next_state some Table} is sat
    noTableNoHaveToBecomeSomeTable : {no Table and not (next_state some Table)} is sat
}

pred tableSwitch {
    always ((no Table implies some Table') and (some Table implies no Table'))
}

test expect afterSwitch {
    noAfterSome : {tableSwitch and some Table and next_state no Table} is sat
    someAfterNo : {tableSwitch and no Table and next_state some Table} is sat
    mustBeNoAfterSome : {tableSwitch and some Table and not (next_state no Table)} is unsat
    mustBeSomeAfterNo : {tableSwitch and no Table and not (next_state some Table)} is unsat
    noAfterAfterNo : {tableSwitch and no Table and next_state next_state no Table} is sat
    someAfterAfterSome : {tableSwitch and some Table and next_state next_state some Table} is sat
    mustBeNoAfterAfterNo : {
        tableSwitch
        no Table
        not (next_state next_state no Table)
    } is unsat
    mustBeSomeAfterAfterSome : {
        tableSwitch
        some Table
        not (next_state next_state some Table)
    } is unsat
    someAfterAfterAfterNo : {
        tableSwitch
        no Table
        next_state next_state next_state some Table
    } is sat
    mustBeSomeAfterAfterAfterNo : {
        tableSwitch
        no Table
        not (next_state next_state next_state some Table)
    } is unsat
    noAfterAfterAfterSome : {
        tableSwitch
        some Table
        next_state next_state next_state no Table
    } is sat
    mustBeNoAfterAfterAfterSome : {
        tableSwitch
        some Table
        not (next_state next_state next_state no Table)
    } is unsat
}
