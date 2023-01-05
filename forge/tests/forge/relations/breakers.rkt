#lang forge

option verbose 0
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

    -- TODO BAD ERRORS: recording here for discussion
    -- Run-spec: contract violation expected: node/formula? given: (Relation l)
    --BAD_ERROR_1: {l} is sat 
    -- expander.rkt:701:40: preds: attribute contains non-syntax value    
    --BAD_ERROR_2: {no l} is sat for {A = `A0 + `A1} is sat

    ----- test "linear" -----

    linearPossible: {} for {next is linear} is sat
    linearnoselfloop: {some a: A | a->a in FrontDesk.next} for {next is linear} is unsat
    linearnoloop: {some a, b: A | (a->b + b->a) in ^(FrontDesk.next)} for {next is linear} is unsat
    linearnobranch: {some disj a, b, c: A | (a->c + b->c) in FrontDesk.next} for {next is linear} is unsat 
    linearnobranch2: {some disj a, b, c: A | (a->b + a->c) in FrontDesk.next} for {next is linear} is unsat 
}

-- While a non "one" parent sig with a binary field can't be broken via bounds, the formula 
--   break version should still be sound. Likewise when `is linear` is composed with partial function.
sig Desk {
    n: set A->A,
    pn: pfunc A->A
}
test expect {
    nonConstantParent_LinearPossible: {} for {n is linear} is sat
    nonConstantParent_linearnoselfloop: {some desk: Desk | some a: A | a->a in desk.n} for {n is linear} is unsat
    nonConstantParent_linearnoloop: {some desk: Desk | some a, b: A | (a->b + b->a) in ^(desk.n)} for {n is linear} is unsat
    nonConstantParent_linearnobranch: {some desk: Desk | some disj a, b, c: A | (a->c + b->c) in desk.n} for {n is linear} is unsat 
    nonConstantParent_linearnobranch2: {some desk: Desk | some disj a, b, c: A | (a->b + a->c) in desk.n} for {n is linear} is unsat 
    
    nonConstantParentPFunc_LinearPossible: {} for {pn is linear} is sat
    nonConstantParentPFunc_linearnoselfloop: {some desk: Desk | some a: A | a->a in desk.pn} for {pn is linear} is unsat
    nonConstantParentPFunc_linearnoloop: {some desk: Desk | some a, b: A | (a->b + b->a) in ^(desk.pn)} for {pn is linear} is unsat
    nonConstantParentPFunc_linearnobranch: {some desk: Desk | some disj a, b, c: A | (a->c + b->c) in desk.pn} for {pn is linear} is unsat 
    nonConstantParentPFunc_linearnobranch2: {some desk: Desk | some disj a, b, c: A | (a->b + a->c) in desk.pn} for {pn is linear} is unsat 
}
