#lang forge

#lang banana

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

--[ TRACE SYNTAX ]--

trace<|S, S_init, S_tran, S_term|> T: plinear {}

--[ IMPLICIT ARGS ]--

--one sig T {
--    init: set S,
--    tran: set S->S,
--    term: set S
--}
--facts[T] T_pred {
--    some tran => {
--        S    = tran.S+S.tran
--        init = tran.S-S.tran
--        term = S.tran-tran.S
--    } else {
--        one   S
--        init = S
--        term = S
--    }
--    all s: init          | S_init[s]
--    all s: S, s': s.tran | S_tran[s, s']
--    all s: term          | S_term[s]
--}
--pred T_fact { all t:T | T_pred[t] }
--inst T_inst { tran is plinear }

--[ BOTH ]--

run<|T|> { S.stuff = A }








