#lang froglet

sig A {
}
sig B {}
sig C {
    // temp: func B -> A -> D
}
sig D {}

pred quants {
    all a_var : A | all b_var: B | all c_var: C | some d_var: D | d_var = d_var
}

// pred quants_expected {
//     all a_var : A | all b_var: B | all c_var: C | c_var.temp[b_var][a_var] = c_var.temp[b_var][a_var]
// }