#lang forge

-- When a <no> quantifier is used with multiple quantifier variables,
-- it desugars into no/no instead of no/some quantifiers;
-- when a <one> quantifier is used with multiple quantifier variables,
-- it desugars into one/one, which doesn't work (no simple correct desugar).
-- Commented out tests in:
--   - forge-alloy/formulas/quantifiedFormulas.rkt
--   - forge-alloy/formulas/multiplicityFormulas.rkt

sig A {friendA: set B}
sig B {friendB: set A}


pred SomePred[a: A, b: B] {
    a->b in friendA and b->a in friendB
}

pred CorrectNo {
    (no a: A, b: B | SomePred[a, b]) iff
    (no a: A | some b: B | SomePred[a, b])
}

pred IncorrectNo {
    (no a: A, b: B | SomePred[a, b]) iff
    (no a: A | no b: B | SomePred[a, b])
}


pred CorrectOne {
    (one a: A, b: B | SomePred[a, b]) iff
    (#{a: A, b: B | SomePred[a, b]} = 1)
}

pred IncorrectOne {
    (one a: A, b: B | SomePred[a, b]) iff
    (one a: A | one b: B | SomePred[a, b])
}


pred CorrectLone {
    (lone a: A, b: B | SomePred[a, b]) iff
    (#{a: A, b: B | SomePred[a, b]} < 2)
}

pred IncorrectLone {
    (lone a: A, b: B | SomePred[a, b]) iff
    (lone a: A | lone b: B | SomePred[a, b])
}

expect noQuantifierBug {
    correctNo : {not CorrectNo} is unsat -- fails
    incorrectNo : {not IncorrectNo} is sat -- fails

    correctOne : {not CorrectOne} is unsat -- fails
    incorrectOne : {not IncorrectOne} is sat -- fails

    correctLone : {not CorrectLone} is unsat -- fails
    incorrectLone : {not IncorrectLone} is sat -- fails
}