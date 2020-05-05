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

    -- incorrect but backwards compatible
    r.Alpha = Beta
    r.Aleph in Beth
    r.Ayy ni Bee

    -- correct (but redundant here)
    Alpha.r = Beta
    Aleph.r in Beth
    Ayy.r ni Bee

    -- Ayy.Bee = Cee // ERROR: inst: one of Ayy or Bee must be a relation
}