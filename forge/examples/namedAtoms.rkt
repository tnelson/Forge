#lang forge

--option verbosity 10

sig A { r: set B }
sig B {}
--sig C extends A {}

run {
--    C in A
--    A ni C
} for {
--    C = Gamma + Gimel + Cee
--    #C <= 3
--    #C = 3
--    A in C + Alpha + Aleph + Ayy
--    B = Beta + Beth + Bee
    
--    r = Alpha->Beta + Aleph->Beth

    A = A0+A1
    B = B0+B1
--    r in A0->B0 + A1->B1
    r.A0 = B0
    r.A1 in B1
}