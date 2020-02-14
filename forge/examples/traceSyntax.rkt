#lang forge

sig A {}
sig S { stuff: set A }

state[S] S_init { no stuff }
transition[S] S_tran {
    some a: A {
        stuff' = stuff+a
        a not in stuff
    }
}
state[S] S_term { #stuff = 3 }

trace<|S, S_init, S_tran, S_term|> T {}
--trace<|S, _, S_tran, _|> T: linear {}
--trace<|S, S_init, S_tran|> T: tree {}
run<|T|> { S.stuff = A }

