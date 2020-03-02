#lang forge

--option verbosity 10

abstract sig A {}
sig B extends A {}
--sig C extends A {}
sig S { stuff: set A }

////

state[S] S_init { no stuff }
transition[S] add_A[a:A] {
    stuff' = stuff+a
    a not in stuff
}
transition[S] S_tran_add {
    some a: A | add_A[this, this', a]
}
transition[S] S_tran_rem {
    some a: A | add_A[this', this, a]
}
state[S] S_term { #stuff = 3 }
state[S] S_inva { some stuff or no stuff }

////

trace<|S, S_init, S_tran_add, S_term|> T: linear {}
trace<|S, S_term, S_tran_rem, S_init|> U: linear {}
run<|T, U|> { S.stuff = A }



