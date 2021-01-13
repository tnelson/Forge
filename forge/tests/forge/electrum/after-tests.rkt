#lang forge

option problem_type temporal
--option verbose 10

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
    CanBeNoEdgesInNextState : {baseProg and after no edges} is sat
    MustBeNoEdgesInNextState : {baseProg and not (after no edges)} is unsat
    //after does not look at current state
    SomeEdgesInCurrentState : {baseProg and some edges} is sat
    NotSomeEdgesAfter : {baseProg and after some edges} is unsat
    //after does not look beyond the next state
    EdgesNotCompleteAfter : {baseProg and after Node->Node in edges} is unsat
    EdgesCompleteEventually : {baseProg and after after Node->Node in edges} is sat
}

var sig Table {}

pred tableSwitch {
    always ((no Table implies some Table') and (some Table implies no Table'))
}


test expect afterSwitch {
    noAfterSome : {tableSwitch and some Table and after no Table} is sat
    someAfterNo : {tableSwitch and no Table and after some Table} is sat
    mustBeNoAfterSome : {tableSwitch and some Table and not (after no Table)} is unsat
    mustBeSomeAfterNo : {tableSwitch and no Table and not (after some Table)} is unsat
    noAfterAfterNo : {tableSwitch and no Table and after after no Table} is sat
    someAfterAfterSome : {tableSwitch and some Table and after after some Table} is sat
    mustBeNoAfterAfterNo : {
        tableSwitch
        no Table
        not (after after no Table)
    } is unsat
    mustBeSomeAfterAfterSome : {
        tableSwitch
        some Table
        not (after after some Table)
    } is unsat
    someAfterAfterAfterNo : {
        tableSwitch
        no Table
        after after after some Table
    } is sat
    mustBeSomeAfterAfterAfterNo : {
        tableSwitch
        no Table
        not (after after after some Table)
    } is unsat
    noAfterAfterAfterSome : {
        tableSwitch
        some Table
        after after after no Table
    } is sat
    mustBeNoAfterAfterAfterSome : {
        tableSwitch
        some Table
        not (after after after no Table)
    } is unsat
}