#lang forge

sig A { r: set A }

pred plinear {
    lone r.A-A.r
    lone A.r-r.A
    no iden & ^r
    all a: A {
        lone a.r
        lone r.a
    }
}

test expect {
    t0: plinear for A = a+b+c+d, r = a->b+b->c is sat
    t1: plinear for A = a+b+c+d, no r is sat
    t2: {} for A = a+b+c+d, r = a->b+b->c, r is plinear is sat
    t3: {} for A = a+b+c+d, no r, r is plinear is sat
    t4: {not plinear} for r is plinear is unsat  -- use bounds breaking
    t5: {not plinear} for r is default, r is plinear is unsat  -- use formula breaking
}

--run plinear

