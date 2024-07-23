#lang froglet

option backend smtlibtor

sig Person {
    parent1 : lone Person,
    parent2 : lone Person,
    spouse  : lone Person
}

pred FamilyFact {
    all p : Person |  {
        p.spouse != p
        not reachable[p,p,parent1,parent2]
        some p.spouse => p.spouse.spouse = p
        not reachable[p,p.spouse,parent1,parent2]
    }
    all disj p1,p2 : Person {      
        reachable[p2,p1,parent1,parent2] => not reachable[p2,p1.spouse,parent1,parent2]
    }
    all disj p1,p2 : Person {
        (reachable[p2, p1.parent1, parent1, parent2] or (p2 = p1.parent1)) => 
            not (reachable[p2, p1.parent2, parent1, parent2] or (p2 = p1.parent2))
    }
}

pred ownGrandparent {
    some p : Person | {
        p.parent1.parent1.spouse = p or
        p.parent1.parent2.spouse = p or
        p.parent1.spouse.parent1 = p or
        p.parent1.spouse.parent1.spouse = p or
        p.parent1.spouse.parent2 = p or
        p.parent1.spouse.parent2.spouse = p or
        p.parent2.parent1.spouse = p or
        p.parent2.parent2.spouse = p or
        p.parent2.spouse.parent1 = p or
        p.parent2.spouse.parent1.spouse = p or
        p.parent2.spouse.parent2 = p or
        p.parent2.spouse.parent2.spouse = p
    }
}

test expect {
    own_grandpa : {FamilyFact and ownGrandparent} is sat
}

run { FamilyFact and ownGrandparent }