#lang forge


sig A { r: set A }
sig B { s: set B }

inst define_s {
    s = B->(B0+B1)  -- sees the B already defined
}
inst myInst {
    r in cotree
    B = B0+B1+B2
    define_s        -- call define_s
}

run { some r } for myInst, 3 <= #A <= 4