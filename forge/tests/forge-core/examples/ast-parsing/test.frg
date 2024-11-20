#lang froglet

sig Person {
    age : one Int,
    parent : lone Person
}

pred age_limits { 
    all p1, p2 : Person | {p1.age <= 5 and p2.age > 2}
}

pred parent_implies { 
    all p: Person | some p.parent => (p.parent != p)
}

pred not_and {
    all p1, p2, p3: Person | not (p1.parent = p3 and p2.parent = p3)
}

pred double_negation {
    all p: Person | not (not (p.age = 3))
}

pred quant_negation {
    not (all p: Person | p.age = 3)
}

pred exists_negation {
    not (some p: Person | p.age = 3)
}

