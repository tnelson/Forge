#lang forge


sig A { r: set A }
sig B { s: set B }
one sig C {}

inst define_s {
    s = B->(B0+B1)  -- sees the B already defined
}
inst myInst {
    r is cotree
    B = B0+B1+B2
    define_s        -- call define_s
}
inst rlin {
    r is linear
}

test expect foo {
    t0: { some r } for exactly myInst, rlin, #A = 4 is sat
    t1: { some r } for exactly myInst, rlin, #A = 4, s is linear is unsat
    t2: { some B } for #B = 0 is unsat -- not exact
    t3: {} for exactly {
        r is tree and r is cotree
        #A=4 and one B and no s and lone C
    } is sat
}

--- WORKING ---
run { some r } for myInst, 3 <= #A <= 4  -- s linearity contraint gone
--run { some B and some C }  -- #B = 0 constraint gone, C still one

--- ERRORS ---
--run {} for exactly #B = 3, #B = 4 -- conflicting int-bounds: no [3, 3] & [4, 4]
--run {} for exactly B=B0+B1+B2, #B = 4 -- conflicting int-bounds: no [3, 3] & [4, 4]
--run { some r } for exactly myInst, 3 <= #A <= 4 -- bounds declared exactly but r not exact