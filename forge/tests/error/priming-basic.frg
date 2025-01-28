#lang forge/temporal
option verbose 0

sig Person {}

one sig Family {
    parent: one Person
}

pred primingNonVar {
    always { Family.parent' = Family.parent } 
}

test expect {
    should_error: {primingNonVar} is sat
}
