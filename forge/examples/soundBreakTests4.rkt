#lang forge

option verbosity 10

sig A { f : set B }
sig B { g : set A, h : set C }
sig C {}

run for {
    f is func
    g is func
    h is func
}


--sig A { f : set B }
--sig B { g : set A }
--
--run for {
--    #A = 2
--    #B = 2
--    f is bij
--    g is bij
--}