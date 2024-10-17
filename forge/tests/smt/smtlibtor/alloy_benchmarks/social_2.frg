#lang forge

option backend smtlibtor
option verbose 0

---------------- Signatures ----------------

abstract sig Person {
	children: set Person,
	parents: set Person,
	siblings: set Person
} 

sig Man, Woman extends Person {}

one sig Helper {
    spouse : pfunc Person -> Person
}
---------------- Functions ----------------


---------------- Predicate ----------------

-- Two persons are blood relatives iff they have a common ancestor
pred BloodRelatives [p: Person, q: Person] {
	some p.*parents & q.*parents
}

---------------- Facts ----------------

pred model_facts {
	parents = ~children
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

	-- A (Helper.spouse) can't be a siblings
	no p: Person | p.(Helper.spouse) in p.siblings

	-- A person can't be married to a blood relative
	no p: Person | BloodRelatives [p, p.(Helper.spouse)]

	-- A person can't have children with a blood relative
	all p, q: Person |
		(some p.children & q.children and p != q) implies
        not BloodRelatives [p, q]
}

---------------- Assertions ----------------

-- No person shares a common ancestor with his (Helper.spouse) (i.e., (Helper.spouse) isn't related by blood). 
pred NoIncest {
	no p: Person | 
		some (p.^parents & p.(Helper.spouse).^parents)
}

test expect {
    social_2 : {model_facts => NoIncest} for 30 is theorem
}




