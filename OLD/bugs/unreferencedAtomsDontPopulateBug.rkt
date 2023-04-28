#lang forge

-- If a sig is not explicitly part of any constraint, then it does not
-- get populated in any instances. The meta-universe is populated 
-- up to the bounds, but the generated instances don't use unused sigs.

option verbosity 0

-- This sig will be explicitly referenced in our predicate;
-- it will end up populating instances
sig A {}

-- This sig will be referenced in an unused predicate; 
-- it will not end up populating any instances
sig B {}

-- These sigs share a relation with each other, which (when
-- translated to KodKod) turns into a constraint;
-- it will end up populating instances
sig C1 { otherC: one C2 }
sig C2 {}

-- This sig is not referenced in any predicate;
-- it will not end up populating any instances
sig D {}


-- This is the predicate we will enforce
pred Used {
    A = A

    -- Note that, since univ and iden currently don't explicitly
    -- reference sigs, this doesn't force B or D to populate
    univ = univ
    iden = iden
}

-- Existence of this predicate does not force B to populate
pred Unused { 
    B = B
}

-- Will only generate the empty instances with A, C1, and C2,
-- when should also generate instances with B and D.
run Used

-- The issue is not with bounds, as can be told from the
-- verbose output, but just to be safe, same thing happens here.
run Used for 4 A, 4 B, 4 C1, 4 C2, 4 D
