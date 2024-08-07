#lang forge

option backend smtlibtor

option verbosity 10

sig Person {
    parents : set Person,
    siblings : set Person,
    mapping : set Person -> Person,
    spouse : lone Person,
    age : one Int
}

pred outside_quant {
    all p: Person | p.siblings = {q: Person | p.parents = q.parents} - p
}

pred inside_quant {
    {p : Person | some q : Person | q in p.parents} = parents.Person
}

pred arity_2_comp {
    all p : Person | p.mapping = {q1, q2: Person | p.parents = q1.parents and p.parents != q2.parents}
}

pred int_comp {
    {i : Int | some p : Person | p.age = i} = sing[1]
}


test expect {
    outside : {outside_quant} is sat   
    inside: {inside_quant} is sat
    arity_2: {arity_2_comp and some mapping} is sat
    int: {int_comp} is sat
}