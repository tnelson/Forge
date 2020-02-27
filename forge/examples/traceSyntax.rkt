#lang forge

sig A {}
one sig B {}
sig S { stuff: set A, b: lone B }

state[S] S_init { no stuff }
transition[S] add_A[a:A] {
    stuff' = stuff+a
    a not in stuff
    b' = b
}
transition[S] S_tran {
    some a: A | add_A[this, this', a]
}
state[S] S_term { #stuff = 3 }
state[S] S_inva { no b }

trace<|S, S_init, S_tran, S_term, S_inva|> T {}
--trace<|S, _, S_tran, _|> T: linear {}
--trace<|S, S_init, S_tran|> T: tree {}
run<|T|> {
    S.stuff = A
--    some B
} for {
    A = apple + orange + peach
--    some B
}

