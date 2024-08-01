#lang forge

option backend smtlibtor

---------------- Signatures ----------------

abstract sig Person {
	children: set Person,
	siblings: set Person
} 

sig Man, Woman extends Person {}

sig Helper {
    spouse : pfunc Person -> Person
}
---------------- Functions ----------------

-- Define the parents relation as an auxiliary one
fun parents : Person -> Person { ~children }

---------------- Facts ----------------

pred model_facts {
	-- No person can be their own ancestor
	no p: Person | p in p.^parents

	-- No person can have more than one father or mother
	all p: Person | (lone (p.parents & Man)) and (lone (p.parents & Woman)) 

	-- A person P's siblings are those people with the same parentss as P (excluding P)
	all p: Person | p.siblings = {q: Person | p.parents = q.parents} - p

	-- Each married man (woman) has a wife (husband) 
	all p: Person | p in (Helper.spouse).Person => {
        let s = p.(Helper.spouse) |
		    (p in Man implies s in Woman) and
		    (p in Woman implies s in Man)
    }

	-- A spouse can't be a siblings
	no p: Person | p in (Helper.spouse).Person => {p.(Helper.spouse) in p.siblings}

}

---------------- Assertions ----------------

-- No person shares a common ancestor with his spouse (i.e., spouse isn't related by blood). 
pred NoIncest {
	no p: Person | p in (Helper.spouse).Person and some p.^parents & p.(Helper.spouse).^parents
}

test expect {
    social_1 : {model_facts => NoIncest} for 30 is theorem
}



