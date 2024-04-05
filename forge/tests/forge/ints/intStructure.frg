#lang forge

option run_sterling off


option verbose 0

pred SuccStructure {
    all i: Int | -- partial function
        lone i.succ

    some i: Int | -- everything reachable from init
        i.*succ = Int

    some i: Int | -- there is a term
        no i.succ
}

test expect SuccessorRelation {
    succStructure1 : SuccStructure for 1 Int is theorem
    succStructure2 : SuccStructure for 2 Int is theorem
    succStructure3 : SuccStructure for 3 Int is theorem
    succStructure4 : SuccStructure for 4 Int is theorem
    succStructure5 : SuccStructure for 5 Int is theorem
}


pred Size1 {
    -- max int 0
    no   sing[0].succ

    -- min int -1
    some succ.(sing[0])
    no   succ.succ.(sing[0])
}

pred Size2 {
    -- max int 1
    some sing[0].succ
    no   sing[0].succ.succ

    -- min int -2
    some succ.succ.(sing[0])
    no   succ.succ.succ.(sing[0])
}

pred Size3 {
    -- max int 3
    some sing[0].succ.succ.succ
    no   sing[0].succ.succ.succ.succ

    -- min int -4
    some succ.succ.succ.succ.(sing[0])
    no   succ.succ.succ.succ.succ.(sing[0])
}

pred Size4 {
    -- max int 7
    some sing[0].succ.succ.succ.succ.succ.succ.succ
    no   sing[0].succ.succ.succ.succ.succ.succ.succ.succ

    -- min int -8
    some succ.succ.succ.succ.succ.succ.succ.succ.(sing[0])
    no   succ.succ.succ.succ.succ.succ.succ.succ.succ.(sing[0])
}

-- Test one above the default
pred Size5 {
    -- max int 15
    some sing[0].succ.succ.succ.succ.succ.succ.succ
        .succ.succ.succ.succ.succ.succ.succ.succ
        
    no   sing[0].succ.succ.succ.succ.succ.succ.succ.succ
        .succ.succ.succ.succ.succ.succ.succ.succ

    -- min int -16
    some  succ.succ.succ.succ.succ.succ.succ.succ.
          succ.succ.succ.succ.succ.succ.succ.succ.(sing[0])

    no succ.succ.succ.succ.succ.succ.succ.succ.
       succ.succ.succ.succ.succ.succ.succ.succ.succ.(sing[0])
}


test expect IntSet {
    size1 : Size1 for 1 Int is theorem
    size2 : Size2 for 2 Int is theorem
    size3 : Size3 for 3 Int is theorem
    size4 : Size4 for 4 Int is theorem
}
