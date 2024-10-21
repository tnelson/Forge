#lang forge

option run_sterling off

option verbose 0

sig Element {}

one sig Wrapper {
    s: pfunc Int -> Element
}

test expect {
    isSeqTest: {
        isSeqOf[Wrapper.s, Element] implies {
            no Wrapper.s or min[Wrapper.s.Element] = 0
        }
    } is checked

    seqLastTest: {
        isSeqOf[Wrapper.s, Element] and some Wrapper.s implies 
          #(Wrapper.s) = add[1, #(seqRest[Wrapper.s])]
    } is checked
}