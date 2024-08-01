#lang forge

option backend smtlibtor

sig Person {
    parents : set Person,
    siblings : set Person
}

pred sib_fact {
    all p: Person | p.siblings = {q: Person | p.parents = q.parents} - p
}

test expect {
    comp_test : {sib_fact} is sat
}