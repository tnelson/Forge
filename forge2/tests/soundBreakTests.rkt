#lang forge

/*
The 'surj symmetry-breaking strategy on X->Y breaks neither X nor Y.
In this case:
    - f: A->B
    - g: B->C
    - h: A->C 
A loop is created, so one of the relations must default to formulas.
This will be the break with the lowest priority, i.e. the one declared last.
Forge should find two instances, corresponding respectively to:
    - h == f.g
    - h != f.g
*/

--sig C {}
sig B {} --g: C }
sig A {
    --f: B,
    --h: C
    f: B
}

fact f: cofunc
--fact f: surj
--fact g: surj
--fact h: surj

run {} for exactly 2 A, exactly 2 B --, exactly 2 C
