#lang forge

pred SuccStructure {
    all i: Int | -- partial function
        lone i.succ

    some i: Int | -- everything reachable from init
        i.*succ = Int

    some i: Int | -- there is a term
        no i.succ
}

test expect SuccessorRelation {
    succStructure1 : {not SuccStructure} for 1 Int is unsat
    succStructure2 : {not SuccStructure} for 2 Int is unsat
    succStructure3 : {not SuccStructure} for 3 Int is unsat
    succStructure4 : {not SuccStructure} for 4 Int is unsat
    succStructure5 : {not SuccStructure} for 5 Int is unsat
}


pred Size1 {
    -- max int 0
    no   sing[0].succ

    -- min int -1
    some succ.     (sing[0])
    no   succ.succ.(sing[0])
}

pred Size2 {
    -- max int 1
    some sing[0].succ
    no   sing[0].succ.succ

    -- min int -2
    some succ.succ.     (sing[0])
    no   succ.succ.succ.(sing[0])
}

pred Size3 {
    -- max int 3
    some sing[0].succ.succ.succ
    no   sing[0].succ.succ.succ.succ

    -- min int -4
    some succ.succ.succ.succ.     (sing[0])
    no   succ.succ.succ.succ.succ.(sing[0])
}

pred Size4 {
    -- max int 7
    some sing[0].succ.succ.succ.succ.succ.succ.succ
    no   sing[0].succ.succ.succ.succ.succ.succ.succ.succ

    -- min int -8
    some succ.succ.succ.succ.succ.succ.succ.succ.     (sing[0])
    no   succ.succ.succ.succ.succ.succ.succ.succ.succ.(sing[0])
}

test expect IntSet {
    size1 : {not Size1} for 1 Int is unsat
    size2 : {not Size2} for 2 Int is unsat
    size3 : {not Size3} for 3 Int is unsat
    size4 : {not Size4} for 4 Int is unsat
}