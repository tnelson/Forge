#lang forge/temporal 

// Relative path from the `library` subdirectory of the Forge package.
open utils/temporal

option max_tracelength 10
option run_sterling off

one sig Counter {
    var val: one Int
}

pred init { Counter.val = 0 }
pred step { Counter.val' = add[Counter.val, 1]}
pred trace { init and always step }

no_vacuity: assert {trace} is sat for 3 Int

nexts_ok_0: assert {repeat_next_state[0, Counter.val = 0] } is necessary for trace for 3 Int
nexts_ok_1: assert {repeat_next_state[1, Counter.val = 1] } is necessary for trace for 3 Int
nexts_ok_3: assert {repeat_next_state[3, Counter.val = 3]} is necessary for trace for 3 Int

primes_ok_0: assert {repeat_prime[0, Counter.val] = 0} is necessary for trace for 3 Int
primes_ok_1: assert {repeat_prime[1, Counter.val] = 1} is necessary for trace for 3 Int
primes_ok_3: assert {repeat_prime[3, Counter.val] = 3} is necessary for trace for 3 Int

test expect {
    nexts_wrong_type:  {repeat_next_state[3, Counter.val]} is forge_error "boolean"
    primes_wrong_type: {repeat_prime[3, Counter.val = 3]} is forge_error "expression"
}