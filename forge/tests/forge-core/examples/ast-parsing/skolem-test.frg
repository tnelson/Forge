#lang froglet


sig A {
    test_rel: lone B
}
sig B {}
sig C {}
sig D {}

pred quants {
    all a_var : A | all b_var: B | all c_var: C | some d_var: D | d_var = d_var
}

pred single {
    all a : A | some b : B | b != a
}
