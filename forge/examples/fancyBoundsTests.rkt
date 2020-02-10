#lang forge


sig A { r: set A }
sig B { s: set B }
one sig C {}

inst define_s {
    s = B->(B0+B1)  -- sees the B already defined
}
inst myInst {
    r in cotree
    B = B0+B1+B2
    define_s        -- call define_s
}

test expect foo {
    t0: { some r } for exactly myInst, #A = 4 is sat
    t1: { some r } for exactly myInst, #A = 4, s in linear is unsat
    t2: { some B } for exactly #B = 0 is unsat
}

run { some r } for myInst, 3 <= #A <= 4  -- s linearity contraint gone

--run { some B and some C }  -- #B = 0 constraint gone, C still one

--run {} for exactly #B = 3, #B = 4 -- conflicting int-bounds: no [3, 3] & [4, 4]
--run {} for exactly B=B0+B1+B2, #B = 4 -- conflicting int-bounds: no [3, 3] & [4, 4]