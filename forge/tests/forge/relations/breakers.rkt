#lang forge

-- Additional tests for lone, one, pfunc, func
sig A, B, C {}
one sig FrontDesk {
    l: lone A,
    p: pfunc A -> B -> C,
    o: one A,
    f: func A -> B -> C,
    next: set A -> A
}

test expect {
    baseline: {} is sat           
    loneCanBeEmpty: {no l} is sat 
    lone2CanBeEmpty: {no l} is sat 
    pfuncCanBeEmpty: {no p} is sat
    pfuncFirstArgCanDifferAndPopulate: {#{a: A | some FrontDesk.p[a]} > 1} is sat
    oneCannotBeEmpty: {no o} is unsat 
    funcCannotBeEmptyUnlessEmptyDomain: {some A and some B and no f} is unsat
    funcFirstArgCanDifferAndPopulate: {#{a: A | some FrontDesk.f[a]} > 1} is sat

    pfuncHigherArityMultipleDenotations: {
        some disj a1, a2 : A | some disj b1, b2 : B | {
            #(FrontDesk.p[a1][b1] +
              FrontDesk.p[a1][b2] +
              FrontDesk.p[a2][b1] +
              FrontDesk.p[a2][b2]) = 4
        }
    } is sat 
    funcHigherArityMultipleDenotations: {
        some disj a1, a2 : A | some disj b1, b2 : B | {
            #(FrontDesk.f[a1][b1] +
              FrontDesk.f[a1][b2] +
              FrontDesk.f[a2][b1] +
              FrontDesk.f[a2][b2]) = 4
        }
    } is sat

    -- Overlap with a previous test, but keep since it checks a valuable syntax foible
    cardinalityCheckSyntax: { {#{a: A | some FrontDesk.p[a]}} > 1} is sat

    -- BAD ERRORS: recording here for discussion
    -- Run-spec: contract violation expected: node/formula? given: (Relation l)
    -- BAD_ERROR_1: {l} is sat 
    -- expander.rkt:701:40: preds: attribute contains non-syntax value    
    -- BAD_ERROR_2: {no l} is sat for {A = `A0 + `A1} is sat

    ----- test "linear" -----

    --linearPossible: {} for {next is linear} is sat


}