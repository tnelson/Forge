#lang forge

option backend smtlibtor
option verbose 0

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

pred existential_comp { 
    some p : Person | {q : Person | q.age = p.age} = p
}

test expect {
    outside : {outside_quant} is sat   
    inside: {inside_quant} is sat
    arity_2: {arity_2_comp and some mapping} is sat
    int: {int_comp} is sat
    existential: {existential_comp} is sat

    -- The set of people for whom there exists someone else with the same age...
    one_sameage_mult: {
        -- cannot be singleton
        not { one {q : Person | some r: Person-q | r.age = q.age} }
    } is checked
    no_sameage_mult: {
        -- can be empty
        no {q : Person | some r: Person-q | r.age = q.age}
    } is sat
    not_always_someone_sameage: {
        not { some p: Person | p in {q : Person | some r: Person-q | r.age = q.age} }
    } is sat
    two_sameage_quant: { 
        some disj p1, p2: Person | p1+p2 = {q : Person | some r: Person-q | r.age = q.age}
    } is sat
    two_equal_sameage_quant: { 
        some p1, p2: Person | p1=p2 and p1+p2 = {q : Person | some r: Person-q | r.age = q.age}
    } is unsat

    -- exists-forall outer quantification testing
    -- There is someone who is a different age from everyone else, 
    -- phrased elaborately via comprehensions. `p1` is not used in the comprehension.

    ea_mixed_outer_sat: {
        some p1: Person | all p2: Person-p1 | {
            p1 not in {q : Person | p2.age = q.age}
        }
    } is sat
    ea_mixed_outer_unsat: {
        #Person > 1
        some p1: Person | all p2: Person-p1 | {
            p1 not in {q : Person | p2.age = q.age}
            p1 in {r : Person | p2.age = r.age}
        }
    } is unsat

    -- Int quantification
    some_int_same: {
        some i: Int | i = {i2: Int | some p: Person | p.age = i2}
    } is sat
    some_int_same_diff: {
        #Int > 1
        all i: Int | i = {i2: Int | some p: Person | p.age = i2}
    } is unsat
}