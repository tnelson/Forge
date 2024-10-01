#lang forge 
option backend smtlibtor 
option verbose 1 

/**
  The semantics of "Int" under smtlibtor are not exactly the same 
  as theory-of-integer-arithmetic in SMT, with theory-of-relations
  added. Rather, because it is the theory of _finite_ relations, 
  there will only ever be a finite set of integers included with 
  any instance. 

  That is, the interpretation of the symbol "Int" is not fixed, 
  but corresponds to the set of integers appearing in some relation
  in the instance being considered.

  This set of tests is meant to illustrate the impact of that 
  distinction, not (only) serve as automated tests.
*/

test expect {
    /**
        If we wrote this in SMT-LIB's non-linear integer arithmetic,
        it would be unsatisfiable: there is no such barrier value! 

        However, if Int is the set of integers used in relations, then
        it behaves similarly to a non-exact bounded sig: the solver
        is free to produce an instance like:
          Int = {10}
          with the $barrier witness = 10 as well.

        the problem is not only vacuous truth, either. Consider:
          Int = {6, 10, 12}
          $barrier = 10.
        Then there is one candidate for "x", 12. And there exists 
        1 < d < 10 in Int: 6. 

        (Note that if you convert this test to a run, you will not 
         see a Skolem witness, because these are not currently reported
         back to Forge from cvc5. To make the Skolem witness visible, 
         you would need to manually convert `barrier` to a field.)
    */
    finitelyManyPrimes: {
        some barrier: Int | all x: Int | (x > barrier) implies {
            some divisor: Int | {
                divisor > 1
                divisor < x 
                remainder[x, divisor] = 0
            }
        }
    } is sat

    /**
      To approach the problem from another perspective, notice
      that the cardinality of Int may vary. With no other constraints,
      and no sig field declarations to force integers to exist, _any_
      size can suffice: 
    */
    noIntegers: {#Int = 0} is sat
    onlyOneInteger: {#Int = 1} is sat
    onlyTwoIntegers: {#Int = 2} is sat
}
