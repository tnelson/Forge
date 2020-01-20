#lang forge

sig A {
    r: set A
}

fact r: func

pred p {
    all a:A { }
}

run p for 16 A

--fun arrr[a: A] : A {
--    a.r.r.r
--}
--fun f[a: A] : A {
--    let x = arrr[A] | let y = arrr[x] | A-y
--}
--query: A = f[A]
--pred p {
--    some A
--    all a:A { arrr[a] = a }
--}
--run {p} for 3 A
