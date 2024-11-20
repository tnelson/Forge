#lang forge

/*
  Factoring a polynomial. This in includes 2 versions:
    - the naive/complicated/unreliable way (universal quantification over Ints)
    - the simple way (existentials only)
  August 2024, Tim
*/

option no_overflow true  -- in case we return to the default engine
option backend smtlibtor -- use the cvc5 theory-of-relations backend

option verbose 3

one sig I {
    root1, root2: one Int
}

test expect {
    -- (x + 123)(x - 321) = x^2 - 198x - 39483 

    -- If we encode the actual equation, we get an overcomplicated version:
    factorPolynomialIntsHardWay: { 
    all x: Int | { 
        // (xi + r1i) * (xi + r2i) 
        multiply[add[x, I.root1], add[x, I.root2]]
        =
        // (xi * xi) + (198 * xi) - 39483))
        subtract[add[multiply[x, x], multiply[198, x]], 39483]
    }} is sat

    -- If we encode what "root" really means, we get a simpler version:
    factorPolynomialEasyWay: {
        0 = subtract[add[multiply[I.root1, I.root1], multiply[198, I.root1]], 39483]
        0 = subtract[add[multiply[I.root2, I.root2], multiply[198, I.root2]], 39483]
        root1 != root2
    } is sat

    -- Should only be ONE such pair
    factorPolynomialEasyWay_unique_solution: {
        0 = subtract[add[multiply[I.root1, I.root1], multiply[198, I.root1]], 39483]
        0 = subtract[add[multiply[I.root2, I.root2], multiply[198, I.root2]], 39483]
        I.root1 != I.root2

        -- Neither root can be 123
        I.root1 != 123 
        I.root2 != 123
        -- Neither root can be -321
        I.root1 != -321
        I.root2 != -321

    } is unsat
}