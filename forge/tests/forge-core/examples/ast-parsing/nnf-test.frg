#lang froglet

sig Person {
    age : one Int,
    parent : lone Person
}

//  *** Basic negation tests ***
pred basic_negation {
    all p: Person | not (p.age = 3)
}

pred double_negation {
    all p: Person | not (not (p.age = 3))
}

pred no_negation {
    all p: Person | no p.parent
}

pred implies_negation { 
    all p: Person | some p.parent => (p.parent != p)
}

pred not_and_negation {
    all p1, p2, p3: Person | not (p1.parent = p3 and p2.parent = p3)
}

pred quant_negation {
    not (all p: Person | p.age = 3)
}

pred exists_negation {
    not (some p: Person | p.age = 3)
}

// *** Complex negation tests ***
pred complex_and {
    all p: Person | not (p.age = 3 and not (p.age = 4 and not (p.age = 5)))
}

pred complex_or {
    all p: Person | not (p.age = 3 or (not (p.age = 4 or p.age = 5)))
}

pred complex_and_or {
    all p: Person | not (p.age = 3 and p.age = 4 or p.age = 5)
}

// *** Quantifier negation tests ***
pred quantifier_negation_all {
    not (all p: Person | p.age = 3)
}

pred quantifier_negation_some {
    not (some p: Person | p.age = 3)
}

pred quantifier_negation_one {
    not (one p: Person | p.age = 3)
}

pred quantifier_negation_lone {
    not (lone p: Person | p.age = 3)
}

// *** Logical equivalence tests ***
pred logical_equivalence {
    all p: Person | (p.age = 3 iff p.age = 3)
}

pred logical_implies {
    all p: Person | (p.age = 3 implies p.parent.age = 4)
    all p: Person | (p.age = 2 implies no p.parent)
}

pred iff_negation_inner {
    all p: Person | (p.age = 3 iff not (p.age != 3))
}

pred iff_negation_outer {
    all p: Person | not (p.age = 3 iff p.age != 3)
}

// *** Checking equivalence ***
pred overall_formula {
    not (some p: Person | p.age = 3)
}

pred expected_nnf {
    all p: Person | p.age != 3
}

pred complex_overall_formula {
    not (some p: Person | p.age = 3 and (all q: Person | q.parent.age > 5))
}

pred complex_expected_nnf {
    all p: Person | p.age != 3 or (some q: Person | q.parent.age <= 5)
}

