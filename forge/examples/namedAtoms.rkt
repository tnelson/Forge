#lang forge

--option verbosity 10

sig A { r: set B }
sig B {}
sig C extends A {}

run {} for {
    C = Gamma + Gimel + Cee
    A = C + Alpha + Aleph + Ayy
    B = Beta + Beth + Bee

    r in (A-C)->B

    r.Alpha = Beta
    r.Aleph in Beth
    r.Ayy ni Bee
}