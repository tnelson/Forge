#lang forge/bsl

/*
  Goldbach's conjecture: every even natural number greater than 2 is the sum of two primes.
  This conjecture is unsolved. It would be a major achievement to prove or disprove it. 

  We can express the conjecture in Forge! But can we trust the result?
*/

pred even[n: Int] {
  remainder[n, 2] = 0
}
pred prime[n: Int] {
  no d: Int | d >= 2 and d < n and remainder[n, d] = 0
}

pred goldbach {
  all n: Int | (n > 2 and even[n]) implies {
    some p1, p2: Int | {
      prime[p1]
      prime[p2]
      n = add[p1,p2]
    }
  }
}

-- Let's write some pointwise tests for our two helper predicates. 
example even0 is {even[0]} for {}
example noteven1 is {not even[1]} for {}
example even2 is {even[2]} for {}
example noteven3 is {not even[3]} for {}
example even4 is {even[4]} for {}

-- Note that our definition calls 1 prime. By convention, it isn't, but it doesn't 
-- matter in this model, so we won't exclude it.
example prime2 is {prime[2]} for {}
example prime3 is {prime[3]} for {}
example composite4 is {not prime[4]} for {}
example prime5 is {prime[5]} for {}
example composite6 is {not prime[6]} for {}

test expect {
    goldbachConjecture: {goldbach} is theorem
}