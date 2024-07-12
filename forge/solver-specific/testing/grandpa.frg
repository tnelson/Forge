#lang froglet

option backend smtlibtor

option verbose 3

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
    // Working simplified example
    // some p, parent: Person | {
    //     p.parent1 = parent or p.parent2 = parent
    //     reachable[p, parent, parent1, parent2, spouse]
    // // --> Starting from parent to reach p should enforce a loop in the family tree
    // }

    // General constraints:
    some p : Person | {
        // Ask students for two of these constraints
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

run {FamilyFact ownGrandparent} for exactly 4 Person