#lang forge

option verbosity 10

sig A { r: set B }
sig B {}
sig C extends A {}

run {
    B in A
    A ni B
} for {
    C = Gamma + Gimel + Cee
--    #C <= 3
--    #C = 3
    A in C + Alpha + Aleph + Ayy
    B = Beta + Beth + Bee
    
    r = Alpha->Beta + Aleph->Beth
--    r = A0->B0 + A1->B1
}