#lang forge

/*
  Expressing the 2024 1710 SMT assignment in Forge. Forge doesn't have reals at the
  moment, but using this to extract the shape of SMT query so I can manually 
  convert it and test how the theory would work in the current engine.

  (1) Pythagorean Triples, Part 1. Forge can prove this (ints) within a bound. 
  (2) Pythagorean Triples, Part 2. Forge can find a counterexample (ints), but it needs a higher bitwidth.
      E.g., n=2,m=4 needs to represent z = (2^2+4^2) = 20, and then to square z = 400, 
      which needs 10 bits: [-512, 511]. 

  (3) Let's try factorization. Ints first:
     (x + 123)(x - 321) = x^2 - 198x - 39483
   
  (4) Let's try factorization in the Reals; this requires copy/paste and replacing 
      sort names in the output SMT (for now), but gives us a good sense of whether 
      it might be straightforward to support Reals. 

  TN Aug 2024
*/

option no_overflow true  -- in case we return to the default engine
option backend smtlibtor -- use the cvc5 theory-of-relations backend

---------------------------------------------------------------------------------------------------

/** 
  x, y, z is a Pythagorean triple if x^2 + y^2 = z^2.
*/
pred isPythagoreanTriple[x,y,z: Int] {
    x > 0 
    y > 0
    z > 0
    let xsq = multiply[x, x], ysq = multiply[y,y], zsq = multiply[z,z] | {
        add[xsq,ysq] = zsq        
    }
}

/**
  A triple is primitive if its elements have no common factors besides 1.
  (Note: the handout is somewhat ambiguous there. Is it pairwise coprime, or collective coprime?)
*/
pred isPrimitive[x, y, z: Int] {
    all d: Int | d > 1 => {
        remainder[x, d] != 0 or
        remainder[y, d] != 0 or
        remainder[z, d] != 0
    }
}

/*
test expect {
    // Generate a triple using the m,n method. 
    pt1: {
        all m, n: Int | (m > n and n > 0) => {
            let msq = multiply[m, m], nsq = multiply[n,n] | {
                isPythagoreanTriple[subtract[msq, nsq], multiply[2, multiply[m, n]], add[msq, nsq]]
            }
        }
    } is theorem

    // Generate a non-primitive triple using the m,n method. 
    pt2: {
        some m, n: Int | let msq = multiply[m, m], nsq = multiply[n,n] | { 
            m > n
            n > 0             
            isPythagoreanTriple[subtract[msq, nsq], multiply[2, multiply[m, n]], add[msq, nsq]]
            not isPrimitive[subtract[msq, nsq], multiply[2, multiply[m, n]], add[msq, nsq]]
        }
    -- In normal Forge, this requires a high bitwidth, so that the squares etc. can be represented.
    -- In SMT, this won't matter (or even be considered by the solver).
    } for 10 Int is sat

}*/

one sig H {
    m: one Int,
    n: one Int
}

-- tests for universally quantified Ints
--test expect {
    /** Find a _primitive_ triple via the m,n method. 
        Note that in contrast to the above, this requires *universal* quantification. */
--    test_with_universals_find_primitive: {
run {
        -- exists-forall, not exists-exists
        (H.m > H.n and H.n > 0) and {
            let msq = multiply[H.m, H.m], nsq = multiply[H.n,H.n] | {
                isPythagoreanTriple[subtract[msq, nsq], multiply[2, multiply[H.m, H.n]], add[msq, nsq]]
                isPrimitive[        subtract[msq, nsq], multiply[2, multiply[H.m, H.n]], add[msq, nsq]]
            }
        }
}

--    } is sat
--}