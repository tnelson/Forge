#lang forge

--sig A { r: set A, s: two A }
--sig B extends A {}
--
--pred p[a:A] { a->a in r }
--fun f[a:A]: A { a.r }
--fun g[a1:A, a2:A]: A {a1+a2}
--
--run { (sing[1]).succ = sing[2] }
--run { some a:A | f[a].r = A }
--check { all a:A | r.s[a] = (r.s)[a] }  -- valid
--check { all a:A | r.s[a] = r.(s[a]) }  -- invalid
--
--state[A] pA {
--    this in r
--    some s
--}
--state[B] pB {
--    this in r
--    some s
--}
--
--run {
--    all a:A | pA[a]
--    all b:B | pB[b]
--}

sig A { r: set A }

run {} for {
    ~r is tree
--    r is tree
    #A = 4
}
