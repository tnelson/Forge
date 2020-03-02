#lang forge

--option verbosity 10

abstract sig A {}
sig B extends A {}
sig C extends A {}
sig S {
    stuff: set A
}

state[S] S_init { no stuff }
transition[S] add_A[a:A] {
    stuff' = stuff+a
    a not in stuff
}
transition[S] S_tran {
    some a: A | add_A[this, this', a]
}
state[S] S_term { #stuff = 3 }
state[S] S_inva { some stuff or no stuff }

trace<|S, _, S_tran, _, S_inva|> T {
    some tran
    all s:init | S_init[s]
    all s:term | S_term[s]
}
--trace<|S, _, S_tran, _|> T: linear {}
--trace<|S, S_init, S_tran|> T: tree {}
run<|T|> {
    S.stuff = A
} for {
--    A = apple + orange + peach
}



