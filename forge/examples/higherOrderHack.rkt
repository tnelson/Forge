#lang forge

sig A {
    r: set A,
    s: set A
}
sig B {}

one sig C { t: set A->B }

--check {
--    A.(r+s) = A.r + A.s
--    A.(r-s) = A.r - A.s
--    A.(r&s) = A.r & A.s
--    all a1, a2: A, b1, b2: B | let x = a1, y = a2 | x != b1 and y != b2
--    A = {a: A | some a}
--}

run {}


--sig Atom {}
--pred union {
--    all s: set Atom |
--        all p: Atom -> Atom |
--            all q: Atom -> Atom |
--                s.(p + q) = s.p + s.q
--    all s: set Atom | some s
--}
--check union for 4 Atom