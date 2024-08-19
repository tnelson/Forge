#lang forge

option backend smtlibtor

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

---------------- Facts ----------------

pred model_facts {
	parents = ~children
	-- No person can be their own ancestor
	no p: Person | p in p.^parents

	-- No person can have more than one father or mother
	all p: Person | (lone (p.parents & Man)) and (lone (p.parents & Woman)) 

	-- A person P's siblings are those people with the same parentss as P (excluding P)
	all p: Person | p.siblings = {q: Person | p.parents = q.parents} - p
	
-- ASSERT (FORALL(p:Atom) : 
--		(TRUE AND (TUPLE(p) IS_IN this_Person)) 
--		=> 
--		(EXISTS(id_13:SET OF [Atom]) : 
--			((FORALL(q:Atom) : 
--			 	(TUPLE(q) IS_IN id_13) 
--				<=> 
--				(({TUPLE(p)} JOIN TRANSPOSE(this_Person_children)) = ({TUPLE(q)} JOIN TRANSPOSE(this_Person_children))))
--			 	 AND 
--			 	 (id_13 <= this_Person)) 
--				AND (({TUPLE(p)} JOIN this_Person_siblings) = (id_13 - {TUPLE(p)}))));	

	-- Each married man (woman) has a wife (husband) 
	all p: Person | let s = p.(Helper.spouse) |
		(p in Man implies s in Woman) and
		(p in Woman implies s in Man)

	-- A spouse can't be a siblings
	no p: Person | p.(Helper.spouse) in p.siblings

}

---------------- Assertions ----------------

-- No person has a parents that's also a siblings.
pred parentsArentsiblings {    
	all p: Person | no p.parents & p.siblings 
}

test expect { 
    social_4 : {model_facts => parentsArentsiblings} for 30 is theorem
}




