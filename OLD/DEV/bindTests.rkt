#lang forge


--sig A { r: set A }
--sig B {}

sig Var {}
sig Formula {}
sig Not {}
sig And {}
sig Or {}
sig Instance {}


test expect {
--    {bind Var = none, Formula = none, Not = none, And = none, Or = none, Instance = none | no Var} is sat
    {bind Var = Var1, Formula = Var1, Not = none, And = none, Or = none, Instance = none | some Var} is sat
}

--run {
--    bind A = A0+A1+A2,
--         r = A->A | some r
--}
