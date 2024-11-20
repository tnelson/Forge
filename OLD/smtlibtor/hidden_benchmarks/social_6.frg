#lang forge

option backend smtlibtor

---------------- Signatures ----------------

abstract sig Person {
	children: set Person,
	siblings: set Person
} 

sig Man, Woman extends Person {}

one sig Helper {
    spouse : pfunc Person -> Person
}
---------------- Functions ----------------

-- Define the parents relation as an auxiliary one
fun parents : Person -> Person { ~children }

---------------- Predicate ----------------

-- Two persons are blood relatives iff they have a common ancestor
pred BloodRelatives [p: Person, q: Person] {
	some p.*parents & q.*parents
}

---------------- Facts ----------------

pred model_facts {

	-- No person can be their own ancestor
	no p: Person | p in p.^parents

	-- No person can have more than one father or mother
	all p: Person | (lone (p.parents & Man)) and (lone (p.parents & Woman)) 

	-- A person P's siblingss are those people with the same parentss as P (excluding P)
	all p: Person | p.siblings = {q: Person | p.parents = q.parents} - p

	-- Each married man (woman) has a wife (husband) 
	all p: Person | let s = p.(Helper.spouse) |
		(p in Man implies s in Woman) and
		(p in Woman implies s in Man)

	-- A spouse can't be a siblings
	no p: Person | p.(Helper.spouse) in p.siblings

	-- A person can't be married to a blood relative
	no p: Person | BloodRelatives [p, p.(Helper.spouse)]

	-- A person can't have children with a blood relative
	all p, q: Person |
		(some p.children & q.children and p != q) implies
        not BloodRelatives [p, q]
}

---------------- Assertions ----------------

-- Every person's siblings are his/her siblings' siblings. 
pred siblingsSiblings {
	all p: Person | p.siblings = p.siblings.siblings
}

test expect {
    social_6: {model_facts => siblingsSiblings} for 30 is theorem
}





