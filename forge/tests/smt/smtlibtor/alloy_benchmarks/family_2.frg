#lang forge

option backend smtlibtor
option verbose 0

abstract sig Person {
	spouse: lone Person,
	parents: set Person
}

sig Men, Women extends Person {}
one sig Adam extends Men {}
one sig Eve extends Women {}

pred Biological {

-- 2 parents: Man and Woman
all p : Person | lone p.parents & Women and lone p.parents & Men

-- No person can be  its ancestor
no p : Person | p in p.^parents
}

pred Social {
-- Symetric spouse
 spouse = ~spouse

-- a spouse cannot be a sibling
no p: Person | p.spouse in p.parents.~parents
}


pred Bible {
-- Adam and Eve married
  Eve in Adam.spouse

-- Adam and Eve have no parents
no (Adam + Eve).parents

-- Except Adam and Eve all others have a mother and a father
    all p: Person - (Adam + Eve)| #p.parents = 2
} 

pred model_facts {
    Biological and Social and Bible
}


pred  NoSelfMarriage {
  -- You can't marry yourself
 no p : Person | p in p.spouse
}
test expect { 
    family_2: {model_facts => NoSelfMarriage} is checked 
}
