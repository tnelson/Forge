#lang forge


sig A { r: set A }
sig B { s: set B }

inst define_s {
    s = B->(B0+B1) 
}
inst myInst {
    r in linear
    B = B0+B1+B2
    define_s
}

--run { some r } for exactly myInst, #A = 4 -- total instance

test expect foo {
    blah: { some r } for exactly myInst, #A = 4 is sat
}

run {} for one A, r=A->A


