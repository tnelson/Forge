#lang froglet

option backend smtlibtor 

abstract sig State {
    top : lone StackElement
}
one sig Initial,Mid,End extends State {}

sig StackElement {
    prev: lone StackElement
}

pred init {
    all s : StackElement | {
        reachable[s,Initial,top,prev] => not reachable[s,s,prev]
    }
    all s : StackElement {
        some st : State {
        reachable[s,st,top,prev]
        }
    }
}
pred InitialToMidPop{
    some Initial.top
    Mid.top = Initial.top.prev
}
pred InitialToMidPush{
    some s : StackElement {
        not reachable[s,Initial,top,prev]
        Mid.top = s
        s.prev = Initial.top
    }
}
pred MidToEndPop {
    some Mid.top
    End.top = Mid.top.prev
}
pred MidToEndPush{
     some s : StackElement {
        not reachable[s,Mid,top,prev]
        End.top = s
        s.prev = Mid.top
    }
}

test expect {
    push_pop : {init and InitialToMidPop and MidToEndPop} is sat
    // push_push : {init and InitialToMidPush and MidToEndPush} is sat
    // pop_push : {init and InitialToMidPop and MidToEndPush} is sat
    // pop_pop : {init and InitialToMidPop and MidToEndPop} is sat
}