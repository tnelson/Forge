#lang forge

sig A { r: set A, s: set A }

pred p[a:A] { a->a in r }
fun f[a:A]: A { a.r }
fun g[a1:A, a2:A]: A {a1+a2}

--run { (sing[1]).succ = sing[2] }
--run { some a:A | f[a].r = A }
--check { all a:A | r.s[a] = (r.s)[a] }  -- valid
--check { all a:A | r.s[a] = r.(s[a]) }  -- invalid

run {
--    some a:A | one A-a
--    some a:A {
--        p[a]
--        one b:A-a {
--            let a = b | p[a]
--        }
--    }
--    two A
    two a: A | p[a]
} for {
--    no r
    no s
}
